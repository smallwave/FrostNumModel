;+
; ;**********************************************************
; Email: wavelet2008@163.com
;
; NAME:
;    D:\workspace\Tech\Code\IDL\DataPreprocessing\Main_ModisLST2DailyLST.pro
; PARAMETERS:
;    See Zhao et al(2014)
;
; Some Path
; Write by :
;    2017-4-14 13:25:53
;;MODIFICATION HISTORY:
;;   Modified and updated  :
;    2017-4-14
;
;+--------------------------------------------------------------------------
;|
;+--------------------------------------------------------------------------
;
PRO MAIN_KIRGINGMODISLST

  COMPILE_OPT idl2
  ;Resore Envi
  ENVI, /RESTORE_BASE_SAVE_FILES
  ENVI_BATCH_INIT, LOG_FILE = logProBatchFile

  ; Define the coefficient array :
  DataPath           =  "D:\Test\"
  OutFilePath        =  "D:\Out\"
  scaleFactor        =   30  ; default set 30 for decrease the consume time
  ;Check file path
  IF(KEYWORD_SET(DataPath) EQ 0 OR KEYWORD_SET(OutFilePath) EQ 0) THEN RETURN

  ; Define log file to record the error such as incomplete data
  logProBatchFile = OutFilePath + 'logFile.txt'
  ; Get the file handle of the logProBatchFile
  GET_LUN, PRO_LUN
  OPENW, PRO_LUN, logProBatchFile
  PRINTF,PRO_LUN, 'Abnormal information are beginning to record........'

  searchMatch = "*.tif"
  searchFiles   =  FILE_SEARCH(DataPath, searchMatch,count=numfiles)
  nFiles        =  N_ELEMENTS(searchFiles)

  FOR n = 0, nFiles-1 DO BEGIN
    
    startFileTime    =  SYSTIME(1)
    filename         =  searchFiles[n]
    ;step1 open file
    ENVI_OPEN_FILE, filename, R_FID = fid
    strFileName      =  FILE_BASENAME(filename)
    IF fid EQ -1 THEN  RETURN
    IF(n EQ 0) THEN BEGIN
      ENVI_FILE_QUERY, fid, DATA_TYPE=DATA_TYPE, NL=NL, NS=NS,dims=dims,NB=NB,OFFSET=offset,INTERLEAVE=interleave
      map_info      = ENVI_GET_MAP_INFO(fid = fid)
      scaleNS         = NS/scaleFactor
      scaleNL         = NL/scaleFactor
    ENDIF
    ;get data 
    temp                = ENVI_GET_DATA(FID = fid,DIMS = dims,pos = 0)
    indexHaveNull       = WHERE(temp EQ .0)
    indexNoNull         = WHERE(temp NE .0)
    temp[indexHaveNull] = mean(temp[indexNoNull])
    
    tempResize          = CONGRID(temp,scaleNS,scaleNL,/INTERP)
    index               = WHERE(tempResize NE .0)
    colX                = (index MOD scaleNS)*scaleFactor
    rowY                = (index / scaleNS)*scaleFactor
    ENVI_CONVERT_FILE_COORDINATES, fid, colX, rowY, x, y, /to_map
    z = tempResize[index]

    ;step2  Create a regular grid
    result = KRIG2D(z, x, y, EXPONENTIAL=[0.5,0.2,1], $
         NX=scaleNS, NY=scaleNL, XOUT=xout, YOUT=yout)

    ;step3 raplace kirging value to orginal LST
    tempReResize            = CONGRID(result,NS,NL,/INTERP)
    temp[indexHaveNull]     = tempReResize[indexHaveNull]

    ;step4  output file
    OutFile  =  OutFilePath +  strFileName
    OPENW, HData, OutFile, /GET_LUN
    WRITEU, HData, temp
    FREE_LUN, HData

    ; Edit the envi header file
    ENVI_SETUP_HEAD, FNAME=OutFile,NS=ns,NL=nl,NB=1,INTERLEAVE=interleave,$
      DATA_TYPE=DATA_TYPE,OFFSET=offset,MAP_INFO=map_info,/WRITE,$
      /OPEN,R_FID=Data_FID

    ; remove all the FIDs in the file lists
    FIDS = ENVI_GET_FILE_IDS()
    FOR i = 0, N_ELEMENTS(FIDS) - 1 DO BEGIN
      IF(FIDS[i] NE -1) THEN  ENVI_FILE_MNG, ID = FIDS[i], /REMOVE
    ENDFOR
    ;print file time consume
    endFileTime = SYSTIME(1)
    STR_INFO  =  'Time consumption at ' + strFileName + ' file is' + STRING(endFileTime-startFileTime)
    PRINT, STR_INFO

  ENDFOR
  FREE_LUN, PRO_LUN
  PRINT, 'Procedure ends at ' + STRING(SYSTIME(/UTC))
  
END
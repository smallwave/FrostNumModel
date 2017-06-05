x = sim(:,1:12)
y = sim(:,13)
epoch_n = 5;
in_fis = genfis2(x,y, 0.5); %生成初始模型
out_fis = anfis(sim,in_fis,epoch_n);

x = test(:,1:12)
y = test(:,13)
datou = evalfis(x,out_fis)
scatter(y,datou)


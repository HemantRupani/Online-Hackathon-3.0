train=read.csv(unzip('train.zip'))
test=read.csv(unzip('test.zip'))
train$urban=as.numeric(train$Var8=='HXYB'|train$Var8=='HXYC'|train$Var8=='HXYD'|train$Var8=='HXYE') #being differ Urban area and non-urban area
train$state1=as.numeric(train$institute_state=='NY') #being differ NY as first class state and other states.
test$urban=as.numeric(test$Var8=='HXYB'|test$Var8=='HXYC'|test$Var8=='HXYD'|test$Var8=='HXYE')
test$state1=as.numeric(test$institute_state=='NY')
traininorder=train[order(-train$Project_Valuation),] #ordered in descending.
trainbenchmark=train[order(train$Similar_Project_Valuation_other_institute),]
#Project_Valuation has different effect for groups a,b,y,z of Similar_Project_Valuation_other_institute
x=trainbenchmark[c(1:1102),] #trainbenchmark<150 --- 1102 entries
y=trainbenchmark[c(1103:33791),]#trainbenchmark between 150 to 300 inclusive --- 32689 entries.
y=y[order(-y$Project_Valuation),] #ordered to remove outliers
y=y[-c(1:6),] # six outliers removed
z=trainbenchmark[c(33792:34397),] #trainbenchmark>300 ---  606 entries.
a=x[c(1:544),] #<128
b=x[c(545:1102),] #128<= <150
testbenchmark=test[order(test$Similar_Project_Valuation_other_institute),]
x1=testbenchmark[c(1:773),]
y1=testbenchmark[c(774:22496),]
z1=testbenchmark[c(22497:22950),]
a1=x1[c(1:396),]
b1=x1[c(397:773),]
regb=lm(b$Project_Valuation~b$Similar_Project_Valuation_other_institute) #its mainly depends on Similar_Project_Valuation_other_institute
regb
regy=lm(y$Project_Valuation~y$Similar_Project_Valuation_other_institute+y$urban+y$state1) #that group depends on all.
regy
regz=lm(z$Project_Valuation~z$Similar_Project_Valuation_other_institute) #mostly depends on(high correlation) Similar_Project_Valuation_other_institute
regz
#regression values added to predict.
a1$Project_Valuation=0 #as 99.08% Project_Valuation was zero in group a
b1$Project_Valuation=2.535*b1$Similar_Project_Valuation_other_institute -313.418
y1$Project_Valuation=0.5408*y1$Similar_Project_Valuation_other_institute+42.407*y1$urban+74.6611*y1$state1+160.6632
z1$Project_Valuation=5.03*z1$Similar_Project_Valuation_other_institute-820.11
final=rbind(a1,b1,y1,z1)
sol=final[,c(1,30)]
write.csv(sol,'sol.csv')

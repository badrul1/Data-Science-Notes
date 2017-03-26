function demoGradDescentCoordTransform(Morth)
% set Morth=1 to use an orthogonal matrix (so that the updates will be the
% same)
% Otherwise set Morth=0 to use a non-orthogonal matrix and have different
% updates in the two coordinate systems

close all
epsilon=0.01; % learning rate

% make a toy problem with some data:
w0=randn(2,1);
N=10;
x=randn(2,N);
y=sin(w0'*x);
w1(:,1)=randn(2,1);
E1(1)=objectivefn(w1(:,1),x,y);

% gradient descent in the w1 space
for i=2:100
    w1(:,i)=w1(:,i-1)-epsilon*gradobjectivefn(w1(:,i-1),x,y);
    E1(i)=objectivefn(w1(:,i),x,y);
end;
plot(E1,'b+')

% gradient descent in the w2 space:
hold on
M=(randn(2,2)); % in general the two updates will be different using this transformation
if Morth==1
M=orth(randn(2,2)); % this makes the updates the same in the two systems
end
invM=inv(M);
w2(:,1)=invM*w1(:,1);
E2(1)=objectivefn(M*w2(:,1),x,y);
for i=2:100
    w2(:,i)=w2(:,i-1)-epsilon*M'*gradobjectivefn(M*w2(:,i-1),x,y);
    E2(i)=objectivefn(M*w2(:,i),x,y);
end
plot(E2,'rx'); title('objective function value')
legend('coordinate system 1','coordinate system 2')

w12=M*w2; % the w2 points mapped back to the w1 space
figure
plot(w1(1,:),w1(2,:),'b+'); title('weight space (in coordinate system 1)')
hold on
plot(w12(1,:),w12(2,:),'rx');
legend('coordinate system 1','coordinate system 2')
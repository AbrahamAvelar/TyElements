addpath 'D:\Dropbox\Scripts Github'

[num txt raw] = xlsread("DeChiara_Domestication_life_traits.xlsx", "Supplementary Table 3");
clsnames=txt(3,16:23) % cls
cls=num(:,16:23)
strain_names=txt(4:end,2)
%%
[num txt raw] = xlsread("Ty_numbers_1011collection.xlsx")

tynames=txt(1,4:end)
Ty=num(:,3:end)
Tynames_strain=txt(2:end,1)
%%

for j=1:13
    figure(j)
    clf
    hold on
    namesx=strain_names;
	namesy=strcat("Ty-",Tynames_strain );
    for i=1:8
        x=cls(:,i);
        y=Ty(:,j);
        labelx = clsnames(i);
        labely = strcat("Ty-",tynames(j+1));
        if i>2
            subplot(3,3,i+1)
        else
            subplot(3,3,i)
        end
        
        CorrelationScatter(x,y,labelx, labely) %[plotHandle r p n m b] =
        
        %CorrelationScatter(x(randperm(1011)),y,labelx, labely)
        
    end
    saveas(gcf,strcat(labelx,labely,'.png'))
end

%%
for j=[7,8,9,12,13]
    figure(j)
    clf
    hold on
    namesx=strain_names;
	namesy=strcat("Ty-",Tynames_strain );
    for i=4
        x=cls(:,i);
        y=Ty(:,j);
        labelx = clsnames(i);
        labely = strcat("Ty-",tynames(j+1));
        
        CorrelationScatter(x,y,labelx, labely) %[plotHandle r p n m b] =
        
        %CorrelationScatter(x(randperm(1011)),y,labelx, labely)
        
    end
%    saveas(gcf,strcat(labelx,labely,'.png'))
end

%%
originalVector = [1, 2, 3, 4, 5];
numElements = numel(originalVector);
randomOrder = randperm(numElements);
randomlyReorderedVector = originalVector(randomOrder);
disp('Original Vector:');
disp(originalVector);
disp('Randomly Reordered Vector:');
disp(randomlyReorderedVector);


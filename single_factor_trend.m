% grid_area = csvread('/Users/shirley/Desktop/Projects/20180620_global/Data/area.csv',0,0);
% % g_gpp = csvread('/Users/shirley/Desktop/Projects/20180620_global/Analysis/single_factor_gpp/global_sf_trend.csv',1,0);
% g_gpp = zeros(35,7);
% g_gpp(1:35,1) = [1982:2016];
% 
% f = dir();
% f = f(5:39);
% for i=1:35
%     factor = csvread(f(i).name,0,0);
%     factor(isnan(factor))=0;
%     
%     glo_gpp = factor .* grid_area;
%     N030_gpp = sum(sum(glo_gpp(121:180,:)))/(1e+15);
%     g_gpp(i,2) = N030_gpp;
% end
% 
%  % csvwrite('/Users/shirley/Desktop/Projects/20180620_global/Analysis/single_factor_gpp/global_sf_trend.csv',g_gpp);
% 

f = csvread('comp_drought.csv');

f= f(1:300,:);

d = imagesc(f,[-2,2]);

set(d,'AlphaData',~isnan(f))

colormap parula(4)
% 
cb = colorbar('Ticks',[-1.5,-0.5,0.5,1.5],...
       'TickLabels',{'negative & significant','negative & insignificant',...
       'positive & insignificant','positive & significant'});

xticks([0 60 120 180 240 300 360 420 480 540 600 660 720]);
xticklabels({'','-150','-120','-90','-60','-30','0','30','60','90','120','150','180'});
yticks([0 60 120 180 240 300]);
yticklabels({'90','60','30','0','-30','-60'});

set(cb,'TickLength',[0]);
title('Impact of drought on GPP Trend (1982-2016)','FontSize',16);

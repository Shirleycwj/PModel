
% a = csvread("1982.csv");
% b = csvread("2016.csv");
% 
% e = b-a;
f = csvread('background_color.csv');
f = f(1:300,:);

g = imagesc(f,[-2,0]);

set(g,'AlphaData',~isnan(f))

colormap gray;

freezeColors;

xlim([0 720]);
ylim([0 300]);

xticks([0 60 120 180 240 300 360 420 480 540 600 660 720]);
xticklabels({'','-150','-120','-90','-60','-30','0','30','60','90','120','150','180'});
yticks([0 60 120 180 240 300]);
yticklabels({'90','60','30','0','-30','-60'});

xlabel('Longtitude');
ylabel('Latitude');

hold on;

%%
e = csvread('/Users/shirley/Desktop/Projects/20180620_global/Analysis/regrssion/multiple_regression/global_grid_modified_noresiduals_direction_lessversion.csv');
e = e(1:300,:);

% figure();

% imshow(e,[0,1],'InitialMagnification','fit');

d = imagesc(e,[-3.5,3.5]);

set(d,'AlphaData',~isnan(e))

colormap parula(7);

% cb = colorbar;

% xticks([0 60 120 180 240 300 360 420 480 540 600 660 720]);
% xticklabels({'','-150','-120','-90','-60','-30','0','30','60','90','120','150','180'});
% yticks([0 60 120 180 240 300]);
% yticklabels({'90','60','30','0','-30','-60'});

cb = colorbar('Ticks',[-3,-2,-1,0,1,2,3],...
       'TickLabels',{'ppfd(-)','temp(-)','CO_2(+)','temp(+)','fAPAR(+)','ppfd(+)','soil moisture(+)'});

set(cb,'TickLength',[0])
     
% cb.Label.String  = 'g C m^{-2} yr^{-1}';

% set(cb,'FontSize',9.5);

% set(get(cb,'Title'),'string','g C m^{-2} yr^{-1}');
% lbpos = get(cb,'title');
% % change Units to data
% set(lbpos,'Units','data');
% set(lbpos,'units','normalized','position',[1.05,1.05]);

title('Most Influential Forcing on GPP Trend','FontSize',16);

%I=getframe(gcf); % 获取坐标系中的图像文件数据
%    
%imwrite(I.cdata,['/Users/shirley/Desktop/most_influential_forcing.tif']);%保存图像为文件

% close all



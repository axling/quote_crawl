-module(qc_text_tv_tests).

-include_lib("eunit/include/eunit.hrl").
-include("quote_crawl.hrl").

-define(PARSE_PAGE_RESULT, 
	[#stock{buy=127.5, 
		sell=127.8,
		name="GETIb",
		volume=1248331,
		highest=128.3,
		lowest=126},
	 #stock{buy=42.4,
		sell=42.5,
		name="FABG",
		volume=1288324,
		highest=42.7,
		lowest=42.1},
	 #stock{buy=71.2,
		sell=71.3,
		name="ERICb",
		volume=26504616,
		highest=71.8,
		lowest=70.1},
	 #stock{buy=70.7,
		sell=71,
		name="ERICa",
		volume=81434,
		highest=71.2,
		lowest=69.6},
	 #stock{buy=170.3,
		sell=170.5,
		name="ELUXb",
		volume=2497691,
		highest=172,
		lowest=167.5},
	 #stock{buy=167.3,
		sell=173.5,
		name="ELUXa",
		volume=1101,
		highest=171.8,
		lowest=167},
	 #stock{buy=137.5,
		sell=137.8,
		name="EKTAb",
		volume=341460,
		highest=138.8,
		lowest=136.3},
	 #stock{buy=66.3,
		sell=66.5,
		name="CAST",
		volume=877833,
		highest=67.3,
		lowest=65.3},
	 #stock{buy=86.7,
		sell=86.8,
		name="BOL",
		volume=11411632,
		highest=86.8,
		lowest=82.5},
	 #stock{buy=308,
		sell=308.5,
		name="AZN",
		volume=1923496,
		highest=311.5,
		lowest=307.5},
	 #stock{buy=196,
		sell=196.5,
		name="AXFO",
		volume=84893,
		highest=197.5,
		lowest=194.5},
	 #stock{buy=85.3,
		sell=85.4,
		name="ATCOb",
		volume=1322286,
		highest=85.5,
		lowest=83.2},
	 #stock{buy=96,
		sell=96.1,
		name="ATCOa",
		volume=6994739,
		highest=96.3,
		lowest=93.8},
	 #stock{buy=118.1,
		sell=118.2,
		name="ASSAb",
		volume=2503972,
		highest=119.9,
		lowest=117.5},
	 #stock{buy=103,
		sell=103.3,
		name="AOIL",
		volume=1501641,
		highest=106.5,
		lowest=103},
	 #stock{buy=249.5,
		sell=250,
		name="ALIV",
		volume=707535,
		highest=250,
		lowest=241},
	 #stock{buy=84,
		sell=84.1,
		name="ALFA",
		volume=2351595,
		highest=84.7,
		lowest=83.5},
	 #stock{buy=151,
		sell=151.5,
		name="ABB",
		volume=3320106,
		highest=152,
		lowest=147.3}]).

-define(PAGE, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><html><head><title>SVT Text - 203 </title><meta http-equiv=\"Content-type\" content=\"text/html; charset=iso-8859-1\" /><meta http-equiv=\"Content-language\" content=\"sv\" /><meta name=\"Author\" content=\"Sveriges Television AB, Stockholm, Sweden\" /><meta name=\"Copyright\" content=\"Sveriges Television AB, Stockholm, Sweden\" /><meta name=\"Robots\" content=\"index, follow, noarchive\" /><meta name=\"Description\" content=\"SVTs Text-TV p\345 internet. Nyheter, Ekonomi, Sport, M\345lservice 377, V\344der, TV... \" /><link href=\"../../css/svttextstyle.css\" rel=\"stylesheet\" type=\"text/css\" /><link href=\"../locals/localstyle.css\" rel=\"stylesheet\" type=\"text/css\" /><script language=\"JavaScript\" type=\"text/javascript\"><!--var nextPage = \"202.html\";var previousPage = \"204.html\";// --></script><script language=\"JavaScript\" src=\"../../script/svttextscript.js\" type=\"text/javascript\"></script><script language=\"JavaScript\" src=\"../locals/localscript.js\" type=\"text/javascript\"></script></head><body onload=\"setFocus('navform','pageinput');\" bgcolor=\"#FFFFFF\"><a name=\"AnchorTop\"></a><div id=\"logo\"><a href=\"100.html\"><img src=\"../../images/logoSvtText.gif\" width=\"146\" height=\"26\" alt=\"SVT Text\" border=\"0\" /></a></div><div id=\"wrapper\"><div id=\"topLine\"></div><ul id=\"menu\"><li><span class=\"mpNumber\"><a href=\"100.html\" class=\"mpNumber\"> 100</a></span><a href=\"100.html\">Nyheter</a></li><li><span class=\"mpNumber\"><a href=\"200.html\" class=\"mpNumber\"> 200</a></span><a href=\"200.html\">Ekonomi</a></li><li><span class=\"mpNumber\"><a href=\"300.html\" class=\"mpNumber\"> 300</a></span><a href=\"300.html\">Sport</a></li><li><span class=\"mpNumber\"><a href=\"400.html\" class=\"mpNumber\"> 400</a></span><a href=\"400.html\">V\344der</a></li><li><span class=\"mpNumber\"><a href=\"500.html\" class=\"mpNumber\"> 500</a></span><a href=\"500.html\">Blandat</a></li><li><span class=\"mpNumber\"><a href=\"600.html\" class=\"mpNumber\"> 600</a></span><a href=\"600.html\">P\345 TV</a></li><li><span class=\"mpNumber\"><a href=\"700.html\" class=\"mpNumber\"> 700</a></span><a href=\"700.html\">Inneh\345ll</a></li><li><span class=\"mpNumber\"><a href=\"800.html\" class=\"mpNumber\"> 800</a></span><a href=\"800.html\">UR</a></li><li id=\"help\"><a href=\"javascript:SgOpenArgs('http://svt.se/svt/jsp/Crosslink.jsp?d=50238','texttvhelp','550','500','status=yes,scrollbars=yes');\">Hj&auml;lp</a> <img src=\"../../images/iconHelp.gif\" width=\"13\" height=\"13\" alt=\"\" border=\"0\" align=\"middle\" /></li></ul><div id=\"topNav\"><span class=\"leftSetting\">Utseende: <a href=\"javascript:settingsNavigate(webLookFolder,203);\" class=\"webView\" title=\"Visa sidan med webbutseende\">Webb</a> | <a href=\"javascript:settingsNavigate(tvLookFolder,203);\" class=\"tvView\" title=\"Visa sidan med TV-utseende\">TV</a></span><div class=\"centerNav\"><form title=\"Navigering till sidnummer\" id=\"navform\" name=\"navform\" action=\"jsp/gotopage.jsp\" onsubmit=\"return formNavigate(this);\"><a href=\"202.html\" class=\"btnBg\"><img onmouseover=\"effect(this);\" onmouseout=\"noEffect(this)\" src=\"../../images/btnBack.gif\" width=\"31\" height=\"18\" alt=\"F\366reg\345ende sida\" border=\"0\" /></a>&nbsp;<input id=\"pageinput\" name=\"pageinput\" type=\"text\" maxlength=\"3\"  title=\"Ange \366nskat sidnummer\" value=\"203\" />&nbsp;<span class=\"btnBg\"><input id=\"submitButton\" onmouseover=\"effect(this);\" onmouseout=\"noEffect(this)\" type=\"image\" src=\"../../images/btnGoToPage.gif\" title=\"G\345 till sida\" value=\"G\345 till sida\" alt=\"G\345 till sida\" /></span>&nbsp;<a href=\"204.html\" class=\"btnBg\"><img onmouseover=\"effect(this);\" onmouseout=\"noEffect(this)\" src=\"../../images/btnForward.gif\" width=\"31\" height=\"18\" alt=\"N\344sta sida\" border=\"0\" /></a></form></div><span class=\"sizeSetting\"><a href=\"javascript:settingsNavigate(normalSizeFolder,203);\" class=\"aNormal\">normal</a> | <a href=\"javascript:settingsNavigate(largeSizeFolder,203);\" class=\"aLarger\">st\366rre</a> | <a href=\"javascript:settingsNavigate(xlargeSizeFolder,203);\" class=\"aLargest\">st\366rst</a></span></div><?xml version=\"1.0\" encoding=\"UTF-8\"?><div><a class=\"preclass\" name=\"subpage1\"> </a><pre class=\"root\"> 203 SVT Text        Torsdag 15 okt 2009\n <span class=\"W\">K\344lla: SIX Telekurs  091014            </span>\n <span class=\"Y bgY DH\"> </span><span class=\"Y bgY DH\"> </span><span class=\"Y bgY DH\"> </span><span class=\"B bgY DH\">LARGE CAP: ABB-GETI       </span><span class=\"R bgY DH\">SLUTKURSER</span>\n <span class=\"Y\">+ </span><span class=\"C\">- </span><span class=\"W\">  K\326P  S\304LJ NAMN SENAST       ANTAL</span>\n <span class=\"G\">                                       </span>\n <span class=\"Y\">5.3   151 151.5 ABB   151.5     3320106</span>\n <span class=\"Y\">1.1    84  84.1 ALFA   84.1     2351595</span>\n <span class=\"Y\">9.5 249.5   250 ALIV  249.5      707535</span>\n <span class=\"C\">0.3   103 103.3 AOIL    103     1501641</span>\n <span class=\"Y\">0.9 118.1 118.2 ASSAb 118.1     2503972</span>\n <span class=\"Y\">3.5    96  96.1 ATCOa  96.1     6994739</span>\n <span class=\"Y\">3.2  85.3  85.4 ATCOb  85.4     1322286</span>\n <span class=\"Y\">  3   196 196.5 AXFO  196.5       84893</span>\n <span class=\"W\">      308 308.5 AZN   308.5     1923496</span>\n <span class=\"Y\">5.8  86.7  86.8 BOL    86.8    11411632</span>\n <span class=\"Y\">1.8  66.3  66.5 CAST   66.5      877833</span>\n <span class=\"Y\">1.3 137.5 137.8 EKTAb 137.8      341460</span>\n <span class=\"Y\">1.3 167.3 173.5 ELUXa   167        1101</span>\n <span class=\"Y\">4.8 170.3 170.5 ELUXb 170.5     2497691</span>\n <span class=\"Y\">1.2  70.7    71 ERICa  70.8       81434</span>\n <span class=\"Y\">1.1  71.2  71.3 ERICb  71.2    26504616</span>\n <span class=\"Y\">0.5  42.4  42.5 FABG   42.4     1288324</span>\n <span class=\"Y\">2.3 127.5 127.8 GETIb 127.8     1248331</span>\n</pre><a class=\"preclass\" name=\"subpage2\"> </a><pre class=\"root sub\"> 203 SVT Text        Torsdag 15 okt 2009\n <span class=\"W\">K\344lla: SIX Telekurs  091014            </span>\n <span class=\"Y bgY DH\"> </span><span class=\"Y bgY DH\"> </span><span class=\"Y bgY DH\"> </span><span class=\"B bgY DH\">LARGE CAP: ABB-GETI       </span><span class=\"R bgY DH\">SLUTKURSER</span>\n <span class=\"Y\">+ </span><span class=\"C\">- </span><span class=\"W\">  K\326P  S\304LJ NAMN SENAST H\326GST L\304GST</span>\n <span class=\"G\">                                       </span>\n <span class=\"Y\">5.3   151 151.5 ABB   151.5   152 147.3</span>\n <span class=\"Y\">1.1    84  84.1 ALFA   84.1  84.7  83.5</span>\n <span class=\"Y\">9.5 249.5   250 ALIV  249.5   250   241</span>\n <span class=\"C\">0.3   103 103.3 AOIL    103 106.5   103</span>\n <span class=\"Y\">0.9 118.1 118.2 ASSAb 118.1 119.9 117.5</span>\n <span class=\"Y\">3.5    96  96.1 ATCOa  96.1  96.3  93.8</span>\n <span class=\"Y\">3.2  85.3  85.4 ATCOb  85.4  85.5  83.2</span>\n <span class=\"Y\">  3   196 196.5 AXFO  196.5 197.5 194.5</span>\n <span class=\"W\">      308 308.5 AZN   308.5 311.5 307.5</span>\n <span class=\"Y\">5.8  86.7  86.8 BOL    86.8  86.8  82.5</span>\n <span class=\"Y\">1.8  66.3  66.5 CAST   66.5  67.3  65.3</span>\n <span class=\"Y\">1.3 137.5 137.8 EKTAb 137.8 138.8 136.3</span>\n <span class=\"Y\">1.3 167.3 173.5 ELUXa   167 171.8   167</span>\n <span class=\"Y\">4.8 170.3 170.5 ELUXb 170.5   172 167.5</span>\n <span class=\"Y\">1.2  70.7    71 ERICa  70.8  71.2  69.6</span>\n <span class=\"Y\">1.1  71.2  71.3 ERICb  71.2  71.8  70.1</span>\n <span class=\"Y\">0.5  42.4  42.5 FABG   42.4  42.7  42.1</span>\n <span class=\"Y\">2.3 127.5 127.8 GETIb 127.8 128.3   126</span>\n</pre></div><div class=\"subWrapper\"><div class=\"subArea\"></div></div><div class=\"clear\" id=\"footerWrapper\"><div id=\"bottomNav\"><span class=\"leftSetting\">Automatisk uppdatering: <a href=\"javascript:settingsNavigate(updateOffFolder,203);\" class=\"updateOff\">Av</a> | <a href=\"javascript:settingsNavigate(updateOnFolder,203);\" class=\"updateOn\">P\345</a></span><div class=\"centerNav\"><a href=\"202.html\" class=\"btnBg\"><img onmouseover=\"effect(this);\" onmouseout=\"noEffect(this)\" src=\"../../images/btnBack.gif\" width=\"31\" height=\"18\" alt=\"F\366reg\345ende sida\" border=\"0\" /></a>&nbsp;<span class=\"upArrow\"><a href=\"#AnchorTop\" class=\"btnBg\"><img onmouseover=\"effect(this);\" onmouseout=\"noEffect(this)\" src=\"../../images/btnUp.gif\" width=\"19\" height=\"18\" alt=\"\" border=\"0\" /></a></span>&nbsp;<a href=\"204.html\" class=\"btnBg\"><img onmouseover=\"effect(this);\" onmouseout=\"noEffect(this)\" src=\"../../images/btnForward.gif\" width=\"31\" height=\"18\" alt=\"N\344sta sida\" border=\"0\" /></a></div></div></div></div><div class=\"clear subWrapper\"><div class=\"copyArea\"><p>&copy; Sveriges Television AB | Anja Hild\351n |  <a href=\"mailto:text@svt.se\">text@svt.se</a></p></div></div><!-- Begin Sitestat code --><script language=JavaScript1.1 type=text/javascript>sitestat(\"http://ld.svt.se/svt/svt/s?svt-text.Ekonomi.203&client=svttext\");</script><noscript><img src=\"http://ld.svt.se/svt/svt/s?svt-text.Ekonomi.203&client=svttext\" width=\"1\" height=\"1\" alt=\"\"></noscript><!-- End Sitestat code --></body></html>").

parse_page_test() ->
    ?assertMatch(?PARSE_PAGE_RESULT, qc_text_tv:parse_page(?PAGE)).



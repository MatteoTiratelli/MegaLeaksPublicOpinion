import pandas as pd
from pytrends.request import TrendReq

# Set up dataframe of search specifications: all nominatim country codes and Google Topic codes for Kim Kardashian, Tax Evasion and Tax Haven

Countries = ["AD","AE","AF","AG","AI","AL","AM","AO","AQ","AR","AS","AT","AU","AW","AX",
"AZ","BA","BB","BD","BE","BF","BG","BH","BI","BJ","BL","BM","BN","BO","BQ","BR","BS","BT",
"BV","BW","BY","BZ","CA","CC","CD","CF","CG","CH","CI","CK","CL","CM","CN","CO","CR","CU",
"CV","CW","CX","CY","CZ","DE","DJ","DK","DM","DO","DZ","EC","EE","EG","EH","ER","ES","ET",
"FI","FJ","FK","FM","FO","FR","GA","GB","GD","GE","GF","GG","GH","GI","GL","GM","GN","GP",
"GQ","GR","GS","GT","GU","GW","GY","HK","HM","HN","HR","HT","HU","ID","IE","IL","IM","IN",
"IO","IQ","IR","IS","IT","JE","JM","JO","JP","KE","KG","KH","KI","KM","KN","KP","KR","KW",
"KY","KZ","LA","LB","LC","LI","LK","LR","LS","LT","LU","LV","LY","MA","MC","MD","ME","MF",
"MG","MH","MK","ML","MM","MN","MO","MP","MQ","MR","MS","MT","MU","MV","MW","MX","MY","MZ",
"NA","NC","NE","NF","NG","NI","NL","NO","NP","NR","NU","NZ","OM","PA","PE","PF","PG","PH",
"PK","PL","PM","PN","PR","PS","PT","PW","PY","QA","RE","RO","RS","RU","RW","SA","SB","SC",
"SD","SE","SG","SH","SI","SJ","SK","SL","SM","SN","SO","SR","ST","SS","SV","SX","SY","SZ",
"TC","TD","TF","TG","TH","TJ","TK","TL","TM","TN","TO","TR","TT","TV","TW","TZ","UA","UG",
"UM","US","UY","UZ","VA","VC","VE","VG","VI","VN","VU","WF","WS","YE","YT","ZA","ZM","ZW"]
SearchCode = ['/m/0261x8t', '/m/03d8n5x', '/m/01z302']
Labels = ['Kim Kardashian', 'Tax Evasion', 'Tax Haven']

pytrends = TrendReq()

for v, l in zip(SearchCode, Labels):
    keyword = [v]
    temp = {}
    for i in Countries:
        try:
            pytrends.build_payload(kw_list=keyword, timeframe="2012-01-01 2020-11-03", geo=i)
            interest_over_time_df = pytrends.interest_over_time()
            temp[i] = interest_over_time_df
            print(f"{l}{i} succesfully downloaded")
        except Exception as e:
            print(f"{l}{i} not downloaded because of the following error:" + str(e))
        continue
    df = pd.concat(temp)
    df.to_csv(f"/Users/matteo/Downloads/{l}.csv")




from pytrends.request import TrendReq
import pytrends
import time
import os
import random
import requests
import os.path

data_dir = "G:\\.shortcut-targets-by-id\\1xFKIqf4LMx9ovfcQgoQSY_fkCY2GWLBg\\2022 Kirpich-Shishkin Google Trends\\data"

attempt_limit = 100

requests_args = {
    "headers": {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:108.0) Gecko/20100101 Firefox/112.0',
        'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8',
        'Accept-Language': 'en-US,en;q=0.5',
        # 'Accept-Encoding': 'gzip, deflate, br',
        'Connection': 'keep-alive',
        # 'Cookie': '__utma=10102256.37975200.1667444735.1673753478.1674063506.15; __utmz=10102256.1671576105.8.4.utmcsr=google|utmccn=(organic)|utmcmd=organic|utmctr=(not%20provided); __utmc=10102256; __utmb=10102256.8.9.1674063608959; __utmt=1; NID=511=ra9fKo72VMHMaOdQDgjXtWSLktgxJ1SwgPoc3Qv6mEpDSe2EUMuA0rozYJoIq5beL396t85Cvi6KA2wPP86wLfUrbkVXahwSBlmCVnLWuP_Jt6-0rRy0VYmpDgXfIA-jhGpkVaXMv5-Zdt5VSBSGZQ46iGft9b0Q9hlx-FyIXI2HjJxg57od5RXivM0bn7NQLr33MhiZIY7x5EwVRsb_3VS317yqJ_oua0cixMKOB39oHubNiUaRo1k--mC6X70J-PGVCIH5McbFcGNDoGe0llTjWkFrI5t_vXMuhwg1VWnZefSc6MT6; 1P_JAR=2023-1-17-23; ANID=AHWqTUlP1qUOdc6934TYEEj92fNkr8YFrDhK9r5xqTUOmWvvBPvKApP_sbms3rdY; SID=SAgeGhT16dM-kr4YDgrBMa4Vz6W4_xrdKQ8J3lc9WgHNRAkS3BNmuJeRUYR799ed-1URNA.; __Secure-1PSID=SAgeGhT16dM-kr4YDgrBMa4Vz6W4_xrdKQ8J3lc9WgHNRAkS6G_Xs0Fa3uynCUb_q_yTDg.; __Secure-3PSID=SAgeGhT16dM-kr4YDgrBMa4Vz6W4_xrdKQ8J3lc9WgHNRAkSSjQsqq09AqwsxSzhgM09UQ.; HSID=AszYBXGxTVjxN1rsZ; SSID=Az-JULugB01kV8-Hd; APISID=cNP57jQgI6g8aNPr/AANSGwOgnphMb1605; SAPISID=cCLSSeA-2zb7HCB2/AdhpU4FBN3Icbh7YM; __Secure-1PAPISID=cCLSSeA-2zb7HCB2/AdhpU4FBN3Icbh7YM; __Secure-3PAPISID=cCLSSeA-2zb7HCB2/AdhpU4FBN3Icbh7YM; SIDCC=AIKkIs2LSlWbszyFJdlFNAy88Sjy9iEgSZiqGdXIS3cSCPUoiJM98CZ0YqpKoN-sL9_4y7PDxXJt; __Secure-3PSIDCC=AIKkIs36w11ePtCQUbbmdUk5Cs8f_kHZ0Omg8y0cpzRIzDQ0oGEME2dQM0LHC8d3RbTu7LNUwuE; SEARCH_SAMESITE=CgQIrJcB; __Secure-1PSIDCC=AIKkIs2OgQtFU37FL56yEUFaQ7vNZLoCMOmG2Yvfbx2zCQPPsbnDcoyxO3_pQwsBzuAmu89frJXy; AEC=ARSKqsIkExb1_6G5dbNnzPMeppnltnihSqcPIfEAEnylgHv5oFkAn-pUx1E; OGPC=19031705-1:19032677-1:19033070-1:; S=billing-ui-v3=WtT89cMNCSLxsUMiKVMMvhIYipQNV9G7:billing-ui-v3-efe=WtT89cMNCSLxsUMiKVMMvhIYipQNV9G7',
        'Upgrade-Insecure-Requests': '1',
        'Sec-Fetch-Dest': 'document',
        'Sec-Fetch-Mode': 'navigate',
        'Sec-Fetch-Site': 'none',
        'Sec-Fetch-User': '?1',
        # Requests doesn't support trailers
        # 'TE': 'trailers',
    }
}

states = [
    "AL",
    "AK",
    "AZ",
    "AR",
    "CA",
    "CO",
    "CT",
    "DE",
    "FL",
    "GA",
    "HI",
    "ID",
    "IL",
    "IN",
    "IA",
    "KS",
    "KY",
    "LA",
    "ME",
    "MD",
    "MI",
    "MN",
    "MS",
    "MO",
    "MT",
    "NE",
    "NV",
    "NH",
    "NJ",
    "NM",
    "NY",
    "NC",
    "ND",
    "OH",
    "OK",
    "OR",
    "PA",
    "RI",
    "SC",
    "SD",
    "TN",
    "TX",
    "UT",
    "VT",
    "VA",
    "WA",
    "WV",
    "WI",
    "WY",
    "DC",
]

kw_names = [
    "covid",
    "coronavirus",
    "covid omicron",
    "covid delta",
    "sars-cov-2",
    "delta variant",
    "omicron variant",
    "covid variant",
]

kw_symptoms = [
    "loss of smell",
    "fever",
    "covid symptoms",
    "symptoms covid",
    "shortness of breath",
    "muscle aches",
    "body aches",
    "headache",
    "cough",
    "fatigue",
]

kw_treatment = [
    "covid testing",
    "covid test",
    "covid treatment",
    "covid medication",
    "covid vaccine",
    "stay home",
    "social distancing",
    "self-isolation",
    "hand sanitizer",
    "covid vaccination",
    "covid home remedies",
    "covid hotline",
]

kw_mortality = [
    "ritual service",
    "memorial service",
    "committal service",
    "visitation service",
    "funeral",
    "funeral expenses assistance",
    "funeral arrangements",
    "bereavement",
    "hearse",
    "coffin",
    "burial",
    "burying",
    "sepulture",
    "cremation",
    "eulogy",
    "obituary",
    "how to write an obituary",
    "obituary templates",
    "casket",
    "urn",
    "ash scattering",
    "immurement",
    "inhumation",
    "wake",
    "grave",
    "funeral liturgy",
    "committal",
    "crematorium",
    "graveyard",
    "grave stone",
    "headstone",
    "monument grave",
    "interment",
    "embalming",
    "embalment",
    "mortuary",
    "death",
    "mourning",
    "funeral arrangements",
    "pallbearer",
    "flowers funeral",
    "funeral flowers",
    "cemetery",
    "how to cope with death",
    "how to cope with loss",
    "how to cope with grief",
    "grieving process and healing",
    "grief support",
    "coroner",

]

keywords = kw_names + kw_symptoms + kw_treatment + kw_mortality

for keyword in keywords:
    kw_list = [keyword]
    for state in states:
        finished = False
        attempt = 1
        while not finished:
            kw = keyword.replace(" ", "_")
            dir_path = f"{data_dir}\\{kw}"
            os.makedirs(dir_path, exist_ok=True)
            path = f"{dir_path}\\{state}.csv"
            try:
                # proxies=["104.223.135.178:10000"]
                proxies = []
                gt_req = TrendReq(hl='en-US', tz=360,
                                  requests_args=requests_args, proxies=proxies)
                gt_req.build_payload(
                    kw_list, cat=0, timeframe='2017-01-01 2023-01-14', geo=f'US-{state}')
                gt_req.interest_over_time().to_csv(path)
                finished = True
            except pytrends.exceptions.ResponseError as err:
                code = err.response.status_code
                print(
                    f"Attempt #{attempt} failed (Code {code}) for {keyword} and {state}. Retrying...")
                attempt += 1
                if attempt > attempt_limit:
                    print(err.response)
                    raise err
            except requests.exceptions.ReadTimeout:
                print(
                    f"Attempt #{attempt} failed (handshake timeout) for {keyword} and {state}. Retrying...")
                time.sleep(10)
                attempt += 1
            else:
                print(
                    f"Attempt #{attempt} was successful for {keyword} and {state}.")
            # if os.path.getsize(path) < 512:
            #     print(f"Attempt #{attempt} failed (file corrupt) for {keyword} and {state}. Retrying...")
            #     finished = False
            time.sleep(30.0 + 60*random.random())

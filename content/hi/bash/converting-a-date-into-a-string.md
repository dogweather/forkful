---
title:                "एक तारीख को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: एक तारीख को स्ट्रिंग में परिवर्तित करना"
simple_title:         "एक तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
डेट को स्ट्रिंग में बदलना, इसका मतलब होता है कि डेट को एक संकेतात्मक रूप में परिवर्तित करना जिसे हम पठन सकते हैं। प्रोग्रामर इसे इसलिए करते हैं ताकि उन्हें अपनी ऐप्लिकेशन में डेट और समय को बेहतर तरीके से प्रदर्शित करने में सहायता मिले।

## कैसे करें:
जब आप Bash में बात करते हैं, `date` command जिसे हम `strftime` ऑप्शन के साथ उपयोग कर सकते हैं, हमें दिनांकों को कस्टम स्ट्रिंग्स में परिवर्तित करने में मदद करता है।

```Bash
#!/bin/bash
date_in_string=$(date '+%Y-%m-%d')
echo "Current date in string format is: $date_in_string"
```

जब आप यह स्क्रिप्ट चलाते हैं, यह उत्तर देगा:

```Bash
Current date in string format is: 2022-11-09
```

## गहरी डाइव
Bash का `date` कमांड Unix time-sharing system से आता है, जिसे 1970 में पहली बार डेवलप किया गया था। 
वैकल्पिक के रूप में, आप Python जैसी अन्य भाषाओं का उपयोग कर सकते हैं, जो आमतौर पर डेट के साथ बेहतर हाथ मिलाती हैं। 
क्रियान्वयन विवरणों के बारे में, `date` command एक समय stamp से विभिन्न प्रारूप में string उत्पन्न करने के लिए `strftime` ऑप्शन का उपयोग करता है।

## और भी देखें 
1. Unix` date` command की अधिक जानकारी के लिए, यहां देखें: [Unix Date Command](https://www.unix.com/man-page/posix/date/1p/)
2. अगर आप Python के साथ डेट को कस्टम स्ट्रिंग में परिवर्तित करना चाहते हैं, तो इसे देखें: [Python Datetime to String](https://docs.python.org/3/library/datetime.html)
---
title:                "एक तारीख को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: एक तारीख को स्ट्रिंग में परिवर्तित करना"
simple_title:         "एक तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक तारीख को स्ट्रिंग में बदलना मतलब की तारीख को पाठ तकनीकी स्वरूप में ढालना। प्रोग्रामर इसे क्योंकि तारीखों पर काम करने के लिए मनमानी तरीके और प्रारूप प्रदान करता है। 

## कैसे करें:
Fish Shell में, आप `string` कमांड का उपयोग करके तारीख को स्ट्रिंग में परिवर्तित कर सकते हैं:

```fish shell
fish> set date (date -u) 
fish> echo $date 
Mon Sep 27 14:07:34 UTC 2021

fish> set str_date (string replace -r ' ' '_' $date)
fish> echo $str_date
Mon_Sep_27_14:07:34_UTC_2021
```

मानक प्रारूप एनवीडीए के साथ तारीख को स्ट्रिंग में बदलने के लिए:

```fish shell
fish> set date (date -u +%Y-%m-%d) 
fish> echo $date 
2021-09-27
```

## गहराई में:
तारीखों को स्ट्रिंग में बदलने की प्राथमिकता हमें तारीखों को छापने, बचाने और कार्यान्वयन के बारे में अधिक नियंत्रण प्रदान करती है। 

इतिहासिक सन्दर्भ में, यह सिस्टम प्रोग्रामिंग का एक महत्वपूर्ण हिस्सा रहा है, जहां तारीखें और समय को अक्सर पाठ फ़ॉर्मेट में देखा जाता है। 

विकल्पों के रूप में, अन्य शेल स्क्रिप्टिंग भाषाओं में जैसे बाश और zsh, भी तारीख को स्ट्रिंग में बदलने की क्षमता प्रदान करते हैं। 

## और भी देखें:
1. Fish Shell डॉक्यूमेंटेशन: https://fishshell.com/docs/current/index.html 
2. डेट और स्ट्रिंग फ़न्क्शंस: https://fishshell.com/docs/current/cmds/string.html 
3. डेट और स्ट्रिंग फ़न्क्शंस (Stackoverflow): https://stackoverflow.com/questions/1401482/yyyy-mm-dd-format-date-in-shell-script
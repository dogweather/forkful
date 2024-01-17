---
title:                "कंप्यूटर प्रोग्रामिंग पर Csv के साथ काम करना"
html_title:           "Elm: कंप्यूटर प्रोग्रामिंग पर Csv के साथ काम करना"
simple_title:         "कंप्यूटर प्रोग्रामिंग पर Csv के साथ काम करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
CSV से काम करना क्या है और कोडर्स इसे क्यों करते हैं?

CSV का उपयोग डेटा को संरचित रूप से रखने और टेक्स्ट फ़ाइल के साथ संचालित करने के लिए होता है। यह आसानी से दृश्यता और शेयर करने में मदद करता है और कोडर्स CSV फ़ाइलों को प्रोसेस करके उनमें स्प्रेडशीट के साथ काम कर सकते हैं।

## कैसे:
CSV के साथ काम कैसे करें?
```Elm
import Csv

myCsv: String
myCsv = "Id,Name,Age
1,John,24
2,Jane,26
3,David,30"

parsedCsv = Csv.Parse.parse myCsv
-- Output: Ok [ [Id, Name, Age], [1, John, 24], [2, Jane, 26], [3, David, 30] ]

-- अब, आप इस CSV डेटा को अपनी Elm ऍप में इस तरह उपयोग कर सकते हैं:
nameAgePairs = List.drop 1 parsedCsv
-- Output: [ [1, John, 24], [2, Jane, 26], [3, David, 30] ]
```

## गहराई में जाएं:
CSV के साथ काम करने के लिए इतिहास, वैकल्पिक विकल्प और इम्प्लीमेंटेशन विवरण के बारे में:
### इतिहास:
CSV का उपयोग सबसे पहले 1972 में इंटरनेट के जनक रॉना ढागटप ने की थी। वे अपने ग्राहकों का बेहतर ध्यान रखने के लिए एक भारी डाटा फॉर्मेट की तलाश में थे। बाद में, यह फॉर्मेट विशेषज्ञों और विकासकर्ताओं के बीच लोकप्रिय हुआ।
### वैकल्पिक विकल्प:
CSV के साथ काम करने के लिए स्टैंडर्ड विकल्प Excel, LibreOffice और Google Sheets समेत कई ऑफ़िस सूट में उपलब्ध है। इनसे पैदा हुए CSV फ़ाइलों को Elm में प्रोसेस करने के लिए, Csv.Parse पैकेज का उपयोग करें।
### इम्प्लीमेंटेशन विवरण:
CSV डेटा स्ट्रिंग नहीं, सॉमन कोड के लिए पर्स स्ट्रिंगों के रूप में प्रदान करता है। आप उपयोग को दृढ़ बनाने के लिए, नए लाइन और कम्मोज्मज तर्क जैसे यूनिकोड लाइब्रेरी को भी उपयोग कर सकते हैं।

## और भी देखें:
CSV को प्रोसेस करने के लिए Csv.Parse पैकेज के साथ अभ्यास करें: https://package.elm-lang.org/packages/elm-explorations/csv/latest/

CSV के बारे में और जानने के लिए: https://www.computerhope.com/jargon/c/csv.htm
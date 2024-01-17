---
title:                "कंप्यूटर प्रोग्रामिंग में csv के साथ काम करना"
html_title:           "Fish Shell: कंप्यूटर प्रोग्रामिंग में csv के साथ काम करना"
simple_title:         "कंप्यूटर प्रोग्रामिंग में csv के साथ काम करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## क्या है और क्यों?
CSV काम करने का एक फॉर्मेट है जो डेटा को rows और columns में स्टोर करता है। प्रोग्रामर्स एक से ज्यादा स्रोतों से जुड़े डेटा को एक साथ सम्मिलित करने के लिए CSV का प्रयोग करते हैं।

## कैसे करें:
```Fish Shell ``` इस्तेमाल करके आप आसानी से CSV फॉर्मेट में डेटा का प्रबंधन कर सकते हैं। निम्नलिखित उदाहरण आपको एक समझाने में मदद करेंगे:

### CSV फ़ाइल से डेटा पढ़ना:
```
set -x data (curl "https://example.com/data.csv" | string split -r --delimiter "\n" )
```
यह उदाहरण आपको CSV फाइल से डेटा पढ़ने की जाँच करने में मदद करेगा।

### CSV फ़ाइल में नया डेटा लिखना:
```
echo "India, Delhi, 11.1" >| data.csv
```
यह उदाहरण आपको CSV फाइल में नया डेटा लिखने की जाँच करने में मदद करेगा।

## डीप डाइव:
CSV का विकास 1972 में IBM ने शुरू किया था। वर्तमान में यह बहुत प्रचलित है क्योंकि यह खुला सॉफ्टवेयर है जिसके कई प्रोग्रामिंग भाषाओं में पोर्ट हुआ है। CSV का एक प्रमुख विकल्प XML है। CSV फ़ाइलें मानक पाठ फ़ाइलें होती हैं जो पाठ की श्रृंखलाओं को कोई नियम नहीं बताती हैं।

## अन्य स्रोत:
यदि आपको CSV फ़ाइलों के साथ और विस्तार से काम करना है, तो आप आसानी से पायथन बिल्ट-इन CSV मॉड्यूल का इस्तेमाल कर सकते हैं। आप CSV मोड्यूल के विशेष लाइब्रेरी को भी जा सकते हैं।

## और भी देखें:
- [फिश शैल दस्तावेज़ीकरण](https://fishshell.com/docs/current/index.html)
- [IBM पर CSV का ब्लॉग पोस्ट](https://www.ibm.com/blogs/labs-in-india/primer-hadoop-csv-format/)
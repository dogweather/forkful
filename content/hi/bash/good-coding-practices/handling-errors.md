---
date: 2024-01-26 00:50:22.811105-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u091C\u092C \u090F\
  \u0915 \u0924\u094D\u0930\u0941\u091F\u093F \u0939\u094B\u0924\u0940 \u0939\u0948\
  \ \u0924\u094B \u0928\u092E\u0942\u0928\u093E \u0906\u0909\u091F\u092A\u0941\u091F\
  ."
lastmod: '2024-04-05T21:53:54.608862-06:00'
model: gpt-4-1106-preview
summary: "\u091C\u092C \u090F\u0915 \u0924\u094D\u0930\u0941\u091F\u093F \u0939\u094B\
  \u0924\u0940 \u0939\u0948 \u0924\u094B \u0928\u092E\u0942\u0928\u093E \u0906\u0909\
  \u091F\u092A\u0941\u091F."
title: "\u090F\u0930\u0930\u094D\u0938 \u0915\u094B \u0939\u0948\u0902\u0921\u0932\
  \ \u0915\u0930\u0928\u093E"
weight: 16
---

## कैसे करें:
```Bash
#!/bin/bash

# stderr को एक फाइल में पुनर्निर्देशित करना
grep "something" file.txt 2> errors.log

# एग्जिट स्टेटस के साथ त्रुटि संचालन
if ! grep "something" file.txt; then
    echo "उप्स, 'something' की खोज करते समय कुछ गलत हो गया।"
    exit 1
fi

# त्रुटि पर निकलने से पहले सफाई के लिए एक ट्रैप का इस्तेमाल
cleanup() {
  echo "अस्थाई फाइलों की सफाई हो रही है..."
  rm temp_*
}

trap cleanup ERR

# इरादतन त्रुटि: फाइल मौजूद नहीं है
cat temp_file.txt
```

जब एक त्रुटि होती है तो नमूना आउटपुट:

```
अस्थाई फाइलों की सफाई हो रही है...
cat: temp_file.txt: इस तरह की कोई भी फाइल नहीं है
```

## गहन अध्ययन
Unix शेल की शुरुआतों से Bash स्क्रिप्टिंग में त्रुटि संचालन मौजूद है, जहाँ सिस्टम प्रशासन और स्वचालन के लिए मजबूत और विश्वसनीय स्क्रिप्ट आवश्यक हैं (और हैं)। परम्परागत रूप से, Bash में त्रुटियों को किसी कमांड की एग्जिट स्टेटस की जांच करके संभाला जाता है, जो कि सफलता के लिए 0 और विफलता के लिए गैर-शून्य मान लौटाता है।

Bash ने `trap` कमांड को बिल्ट-इन के रूप में पेश किया, जो उपयोगकर्ताओं को विभिन्न सिग्नलों या स्क्रिप्ट निकलने पर चलाने के लिए कमांड्स निर्दिष्ट करने की अनुमति देता है। यह सफाई कार्यों या अंतिम सहारा त्रुटि संचालन तंत्र के लिए उपयोगी है।

`set` कमांड भी है, जो Bash के त्रुटियों पर व्यवहार को बदल सकती है। उदाहरण के लिए, `set -e` एक स्क्रिप्ट को तुरंत बाहर निकाल देगा अगर कोई भी कमांड गैर-शून्य स्टेटस के साथ बाहर निकलती है, जल्दी असफल होने का एक तरीका है और बढ़ती त्रुटियों को बचने का एक उपाय है।

Bash के बिल्ट-इन त्रुटि संचालन के विकल्पों में फाइलों के अस्तित्व की विस्तार से जांच, कमांड प्रतिस्थापन का इस्तेमाल, या यहां तक कि आपके स्वयं के फंक्शंस लिखना शामिल है ताकि त्रुटियों को और अधिक बारीकि से संभाला जा सके।

हालांकि कठोर त्रुटि संचालन कभी-कभी छोटी स्क्रिप्टों के लिए अत्यधिक लग सकता है, यह एक ऐसी प्रथा है जो डिबगिंग में बहुत समय बचा सकती है और आपके और उपयोगकर्ताओं दोनों के लिए अप्रत्याशित व्यवहार से बचा सकती है।

## इसे भी देखें
- Bash मैन्युअल शेल पैरामीटर्स पर: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
- त्रुटि संचालन पर एडवांस्ड Bash-स्क्रिप्टिंग गाइड का एक भाग: https://www.tldp.org/LDP/abs/html/exit-status.html
- `trap` पर एक गहन गाइड: https://mywiki.wooledge.org/SignalTrap

याद रखें, स्क्रिप्टिंग एक कला रूप है, और आप जिस तरह से फिसलन और अस्थिरता को संभालते हैं, वह आपकी कृति को और अधिक लचीला बना सकता है। हैप्पी स्क्रिप्टिंग!

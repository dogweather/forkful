---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:24.245583-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Bash HTML \u092A\u093E\
  \u0930\u094D\u0938\u093F\u0902\u0917 \u0915\u0947 \u0932\u093F\u090F \u092A\u0939\
  \u0932\u0940 \u092A\u0938\u0902\u0926 \u0928\u0939\u0940\u0902 \u0939\u094B\u0924\
  \u0940, \u0932\u0947\u0915\u093F\u0928 `grep`, `awk`, `sed`, \u092F\u093E \u092C\
  \u093E\u0939\u0930\u0940 \u0909\u092A\u0915\u0930\u0923\u094B\u0902 \u091C\u0948\
  \u0938\u0947 `lynx` \u0915\u0947 \u0938\u093E\u0925 \u0907\u0938\u0947 \u0915\u0930\
  \u0928\u093E \u0938\u0902\u092D\u0935 \u0939\u0948\u0964 \u0920\u094B\u0938\u0924\
  \u093E \u0915\u0947 \u0932\u093F\u090F,\u2026"
lastmod: '2024-03-13T22:44:52.622567-06:00'
model: gpt-4-0125-preview
summary: "Bash HTML \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917 \u0915\u0947\
  \ \u0932\u093F\u090F \u092A\u0939\u0932\u0940 \u092A\u0938\u0902\u0926 \u0928\u0939\
  \u0940\u0902 \u0939\u094B\u0924\u0940, \u0932\u0947\u0915\u093F\u0928 `grep`, `awk`,\
  \ `sed`, \u092F\u093E \u092C\u093E\u0939\u0930\u0940 \u0909\u092A\u0915\u0930\u0923\
  \u094B\u0902 \u091C\u0948\u0938\u0947 `lynx` \u0915\u0947 \u0938\u093E\u0925 \u0907\
  \u0938\u0947 \u0915\u0930\u0928\u093E \u0938\u0902\u092D\u0935 \u0939\u0948\u0964\
  \ \u0920\u094B\u0938\u0924\u093E \u0915\u0947 \u0932\u093F\u090F, \u0939\u092E `libxml2`\
  \ \u092A\u0948\u0915\u0947\u091C \u0938\u0947 `xmllint` \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0930\u0947\u0902\u0917\u0947\u0964."
title: "HTML \u0935\u093F\u0936\u094D\u0932\u0947\u0937\u0923"
weight: 43
---

## कैसे करें:
Bash HTML पार्सिंग के लिए पहली पसंद नहीं होती, लेकिन `grep`, `awk`, `sed`, या बाहरी उपकरणों जैसे `lynx` के साथ इसे करना संभव है। ठोसता के लिए, हम `libxml2` पैकेज से `xmllint` का उपयोग करेंगे।

```bash
# यदि आवश्यक हो तो xmllint स्थापित करें
sudo apt-get install libxml2-utils

# नमूना HTML
cat > sample.html <<EOF
<html>
<head>
  <title>नमूना पेज</title>
</head>
<body>
  <h1>नमस्ते, Bash!</h1>
  <p id="myPara">Bash मुझे पढ़ सकता है।</p>
</body>
</html>
EOF

# शीर्षक पार्स करें
title=$(xmllint --html --xpath '//title/text()' sample.html 2>/dev/null)
echo "शीर्षक है: $title"

# आईडी द्वारा पैराग्राफ निकालें
para=$(xmllint --html --xpath '//*[@id="myPara"]/text()' sample.html 2>/dev/null)
echo "पैराग्राफ सामग्री है: $para"
```

आउटपुट:
```
शीर्षक है: नमूना पेज
पैराग्राफ सामग्री है: Bash मुझे पढ़ सकता है।
```

## गहराई से
पहले के दिनों में, प्रोग्रामर HTML को स्कैन करने के लिए `grep` जैसे regex-आधारित उपकरणों का उपयोग करते थे, लेकिन वह अव्यावहारिक था। HTML नियमित नहीं होता—वह संदर्भिक होता है। पारंपरिक उपकरण इसे मिस कर देते हैं और त्रुटि से भरे हो सकते हैं।

विकल्प? ढेर सारे। पायथन के साथ Beautiful Soup, PHP के साथ DOMDocument, जावास्क्रिप्ट के साथ DOM पार्सर्स—HTML की संरचना को समझने के लिए डिज़ाइन की गई भाषाएँ और लाइब्रेरी।

बैश स्क्रिप्ट में `xmllint` का उपयोग सरल कार्यों के लिए मजबूत होता है। यह XML को समझता है, और इस विस्तार से, XHTML भी। नियमित HTML अप्रत्याशित हो सकता है, भले ही। यह हमेशा XML के सख्त नियमों का पालन नहीं करता। `xmllint` HTML को XML मॉडल में जबरदस्ती डालता है जो अच्छी तरह से बनाई गई HTML के लिए अच्छा काम करता है लेकिन अव्यवस्थित सामग्री पर ठोकर खा सकता है।

## यह भी देखें
- [W3Schools - HTML DOM Parser](https://www.w3schools.com/xml/dom_intro.asp): HTML DOM को समझाना।
- [MDN Web डॉक्स - पार्सिंग और एक्सएमएल को सीरियलाइज़ करना](https://developer.mozilla.org/en-US/docs/Web/Guide/Parsing_and_serializing_XML): XHTML के लिए लागू हो सकने वाले XML पार्सिंग सिद्धांतों के लिए।
- [Beautiful Soup डॉक्युमेंटेशन](https://www.crummy.com/software/BeautifulSoup/bs4/doc/): HTML पार्सिंग के लिए पायथन लाइब्रेरी।
- [libxml2 डॉक्युमेंटेशन](http://xmlsoft.org/): `xmllint` और संबंधित XML उपकरणों पर विवरण।

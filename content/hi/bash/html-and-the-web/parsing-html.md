---
title:                "HTML विश्लेषण"
aliases: - /hi/bash/parsing-html.md
date:                  2024-02-03T19:12:24.245583-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML विश्लेषण"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

HTML पार्सिंग का मतलब होता है HTML फाइल की संरचना और सामग्री के माध्यम से जानकारी निकालना। प्रोग्रामर इसे डेटा प्राप्त करने, सामग्री में परिवर्तन करने, या वेबसाइट्स से डेटा खुरचने के लिए करते हैं।

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

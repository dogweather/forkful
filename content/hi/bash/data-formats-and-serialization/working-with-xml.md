---
date: 2024-01-26 04:28:35.329386-07:00
description: "\u0915\u0948\u0938\u0947: \u092F\u0939\u093E\u0901 Bash \u092E\u0947\
  \u0902 XML \u0915\u094B \u092A\u093E\u0930\u094D\u0938 \u0915\u0948\u0938\u0947\
  \ \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u0909\u092A\
  \u0915\u0930\u0923? xmllint \u0914\u0930 xmlstarlet\u0964 XML \u0924\u0924\u094D\
  \u0935\u094B\u0902 \u0915\u0947 \u092E\u093E\u0927\u094D\u092F\u092E \u0938\u0947\
  \ \u0932\u0942\u092A\u093F\u0902\u0917? \u0905\u0935\u0936\u094D\u092F\u0964 \u0928\
  \u092E\u0942\u0928\u093E \u0906\u0909\u091F\u092A\u0941\u091F \u0915\u0947 \u0938\
  \u093E\u0925 \u0909\u0926\u093E\u0939\u0930\u0923."
lastmod: '2024-04-05T22:40:40.338231-06:00'
model: gpt-4-0125-preview
summary: "\u092F\u0939\u093E\u0901 Bash \u092E\u0947\u0902 XML \u0915\u094B \u092A\
  \u093E\u0930\u094D\u0938 \u0915\u0948\u0938\u0947 \u0915\u093F\u092F\u093E \u091C\
  \u093E\u0924\u093E \u0939\u0948\u0964 \u0909\u092A\u0915\u0930\u0923?"
title: "XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 40
---

## कैसे:
यहाँ Bash में XML को पार्स कैसे किया जाता है। उपकरण? xmllint और xmlstarlet। XML तत्वों के माध्यम से लूपिंग? अवश्य। नमूना आउटपुट के साथ उदाहरण:

```bash
# मान लें कि xmlstarlet स्थापित है
# स्थापित करें: apt-get install xmlstarlet

# XML सामग्री को पार्स करना
cat <<EOF > sample.xml
<fruits>
  <fruit name="Apple"/>
  <fruit name="Banana"/>
</fruits>
EOF

# xmlstarlet के साथ नाम निकालें
xmlstarlet sel -t -m "//fruit" -v "@name" -n sample.xml

# आउटपुट होना चाहिए:
# Apple
# Banana
```

## गहराई में
90 के दशक में, XML SGML के एक सरल विकल्प के रूप में उभरा, पर HTML से अधिक संरचित था। अब, इसके साथ कंपनी है - उदाहरण के लिए JSON, YAML। पर XML अभी भी टिक हुआ है, खासकर कॉन्फिग्स और SOAP-आधारित वेब सेवाओं में।

उपकरण के हिसाब से, xmllint XML मान्यकरण, xpath क्वेरीज के लिए आरामदायक है। xmlstarlet XML शेनानिगन्स के लिए स्विस-आर्मी चाकू है - क्वेरी, संपादित, मान्य करें, रूपांतरित करें। Bash स्क्रिप्ट में, वे XML कार्यों के लिए सुपरहीरो हैं।

अंदर की बात, xmllint libxml2 का उपयोग करता है - XML C पार्सर। यह तेज़ है, पर त्रुटि संदेश? रहस्यमय। और xmlstarlet? पुनरावृत्ति टेम्प्लेट और EXSLT समर्थन। मन को मोड़ देने वाला, पर शक्तिशाली।

## देखें भी
- [xmlsoft.org](http://xmlsoft.org/): Libxml2 और xmllint सामग्री।
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash): वास्तविक दुनिया की समस्याएँ और समाधान।
- [W3Schools XML ट्यूटोरियल](https://www.w3schools.com/xml/): XML की मूल बातें।

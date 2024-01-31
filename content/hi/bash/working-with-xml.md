---
title:                "XML के साथ काम करना"
date:                  2024-01-26T04:28:35.329386-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"

category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/working-with-xml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
XML के साथ काम करने में पार्सिंग, निकालना, और डाटा को प्रसारणीकरण अंकन भाषा (एक्सटेंसिबल मार्कअप लैंग्वेज) प्रारूप में हेरफेर करना शामिल है। प्रोग्रामर XML के साथ जूझते हैं क्योंकि यह कॉन्फिग्स, API, और अधिक के लिए एक व्यापक डाटा आदान-प्रदान प्रारूप है।

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

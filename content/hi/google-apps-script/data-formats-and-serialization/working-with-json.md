---
title:                "JSON के साथ काम करना"
aliases: - /hi/google-apps-script/working-with-json.md
date:                  2024-02-01T22:07:10.738263-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/google-apps-script/working-with-json.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

JSON, या JavaScript Object Notation, एक हल्का प्रारूप है डेटा को संग्रहित और परिवहन करने के लिए, जो सर्वर-से-क्लाइंट संचार और कॉन्फ़िगरेशन फ़ाइलों के लिए आदर्श है। प्रोग्रामर इसका उपयोग Google Apps Script में करते हैं ताकि Google सेवाओं (जैसे Sheets, Docs, Drive) और बाहरी स्रोतों के बीच आसानी से डेटा का आदान-प्रदान हो सके, इसकी मानव-पढ़ने-योग्य संरचना और JavaScript-आधारित पर्यावरणों के साथ आसान एकीकरण के कारण।

## कैसे:

Google Apps Script में JSON का प्रबंधन एक सरल प्रक्रिया है, ज्यादातर JSON पार्सिंग और स्ट्रिंगीफ़ाई करने के लिए JavaScript द्वारा प्रदान किए गए मूल समर्थन के कारण। यहाँ कुछ सामान्य ऑपरेशन हैं:

**1. JSON पार्सिंग**: मान लें हम एक वेब सेवा से एक JSON स्ट्रिंग प्राप्त करते हैं; इसे एक JavaScript ऑब्जेक्ट में पार्स करना डेटा के हेरफेर के लिए आवश्यक है।

```javascript
var jsonString = '{"name": "Sample Project", "version": "1.0.0"}';
var obj = JSON.parse(jsonString);
Logger.log(obj.name); // आउटपुट: Sample Project
```

**2. JavaScript ऑब्जेक्ट्स को स्ट्रिंगीफाई करना**: इसके विपरीत, एक JavaScript ऑब्जेक्ट को एक JSON स्ट्रिंग में परिवर्तित करना उस समय उपयोगी होता है जब हमें Apps Script से किसी बाहरी सेवा में डेटा भेजने की आवश्यकता होती है।

```javascript
var projectData = {
  name: "Sample Project",
  version: "1.0.0"
};
var jsonString = JSON.stringify(projectData);
Logger.log(jsonString); // आउटपुट: '{"name":"Sample Project","version":"1.0.0"}'
```

**3. जटिल डेटा के साथ काम करना**:
अधिक जटिल डेटा संरचनाओं, जैसे कि ऑब्जेक्ट्स के सरणियों के लिए, प्रक्रिया वही रहती है, JSON की डेटा प्रस्तुति के लिए लचीलेपन का प्रदर्शन करती है।

```javascript
var projects = [
  {name: "Project 1", version: "1.0"},
  {name: "Project 2", version: "2.0"}
];
var jsonString = JSON.stringify(projects);
Logger.log(jsonString); // आउटपुट: '[{"name":"Project 1","version":"1.0"},{"name":"Project 2","version":"2.0"}]'
```

## गहराई से समझे

JSON की आधुनिक वेब अनुप्रयोगों में व्यापकता को कम नहीं आंका जा सकता, इसकी सादगी में निहित है और यह कैसे बिना किसी परेशानी के JavaScript के साथ एकीकृत होता है, वेब की भाषा। इसकी डिज़ाइन, जो JavaScript ऑब्जेक्ट शाब्दिक अभिव्यक्तियों से प्रेरित है, हालांकि सख्त है, इसकी त्वरित स्वीकार्यता को सुविधाजनक बनाती है। 2000 के दशक की शुरुआत में, AJAX-प्रवेण वेब अनुप्रयोगों के लिए XML के विकल्प के रूप में JSON लोकप्रिय हुआ, जो एक अधिक हल्का और कम शब्दों वाला डेटा विनिमय प्रारूप प्रदान करता है। विभिन्न Google APIs और बाहरी सेवाओं के साथ Google Apps Script के गहरे एकीकरण को देखते हुए, JSON इन मंचों पर डेटा की संरचना, परिवहन, और हेरफेर के लिए एक केंद्रीय प्रारूप के रूप में काम करता है।

जबकि JSON वेब अनुप्रयोगों के लिए सर्वोच्च स्थान पर है, कॉन्फ़िगरेशन फ़ाइलों के लिए YAML जैसे वैकल्पिक डेटा प्रारूप या उच्च-प्रदर्शन वातावरणों में अधिक कुशल बाइनरी सीरियलाइज़ेशन के लिए Protobuf जैसे मौजूद हैं। हालांकि, JSON का पठनीयता, उपयोग में आसानी, और प्रोग्रामिंग भाषाओं और टूल्स के विस्तृत समर्थन का संतुलन इसे Google Apps Script और उससे आगे के विकासकर्ताओं के लिए एक डिफ़ॉल्ट विकल्प के रूप में इसकी स्थिति को मजबूत करता है।

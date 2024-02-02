---
title:                "स्ट्रिंग को लोअर केस में बदलना"
date:                  2024-02-01T21:52:43.132849-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग को लोअर केस में बदलना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/google-apps-script/converting-a-string-to-lower-case.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Google Apps Script में स्ट्रिंग को लोअर केस में परिवर्तित करना, Google उत्पादों के भर में कार्यों का स्वचालन करने के लिए एक क्लाउड-आधारित स्क्रिप्टिंग भाषा, एक मौलिक कार्य है जिसका उद्देश्य पाठ डेटा को मानकीकृत करना है। प्रोग्रामर अक्सर उपयोगकर्ता इनपुट में समानता, डेटा संसाधन, या स्ट्रिंग्स की तुलना करते समय, मामला संवेदनशीलता मुद्दों को खत्म करने के लिए, इस कार्रवाई को निष्पादित करते हैं।

## कैसे:

Google Apps Script में स्ट्रिंग को लोअर केस में परिवर्तित करना सरल है, धन्यवाद स्क्रिप्टिंग वातावरण के भीतर उपलब्ध निर्मित JavaScript विधियों को। जिस `toLowerCase()` विधि का आप अधिकतर उपयोग करेंगे, यहाँ है कि आप इसे कैसे लागू कर सकते हैं:

```javascript
function convertToLower() {
  var originalString = "Hello, WORLD!";
  var lowerCaseString = originalString.toLowerCase();
  
  Logger.log(lowerCaseString); // आउटपुट: hello, world!
}
```

यह सरल फ़ंक्शन एक मूल स्ट्रिंग लेने, `toLowerCase()` विधि लागू करने और परिणाम की लॉगिंग करने का विद्यमान करता है। यह विशेष रूप से उन इनपुटों के साथ निपटने में उपयोगी है जिन्हें मामला-असंवेदनशील होना आवश्यक है। उदाहरण के लिए, उपयोगकर्ताओं द्वारा विभिन्न केस में इनपुट किए गए ईमेल पते की तुलना करना।

इसके अलावा, जब आप ऐरे डेटा के साथ काम कर रहे होते हैं, तो आप प्रत्येक तत्व को लोअर केस में परिवर्तित करने के लिए मैप कर सकते हैं:

```javascript
function convertArrayItemsToLower() {
  var namesArray = ["Alice", "BOB", "Charlie"];
  var lowerCaseNamesArray = namesArray.map(function(name) {
    return name.toLowerCase();
  });
  
  Logger.log(lowerCaseNamesArray); // आउटपुट: [alice, bob, charlie]
}
```

यह उदाहरण स्ट्रिंग डेटा के बहुसंख्यकों के साथ निपटने में `toLowerCase()` की बहुमुखी प्रतिभा को बल देता है, आपके डेटासेट भर में समरूपता सुनिश्चित करता है।

## गहन विवेचन

`toLowerCase()` विधि, जो JavaScript से उत्तराधिकारित है और Google Apps Script के भीतर उपयोग की जाती है, मूल रूप से जावास्क्रिप्ट के शुरुआती संस्करणों से स्ट्रिंग मैनिपुलेशन का एक अनिवार्य भाग रही है। इसका मुख्य उद्देश्य पाठ्य डेटा के मामला-असंवेदनशील संभालने में सहायता करना है, जिसकी आवश्यकता गतिशील, उपयोगकर्ता-इंटरेक्टिव वेब अनुप्रयोगों के आगमन के साथ उठी थी। इसकी सादगी के बावजूद, तंत्र डाटा सत्यापन, छंटाई, और खोज एल्गोरिदम में मामला संवेदनशीलता द्वारा पेश की गई जटिलता को कम करके एक महत्वपूर्ण भूमिका निभाता है।

प्रदर्शन के संदर्भ में, परिवर्तन प्रक्रिया आधुनिक JavaScript इंजनों में अत्यधिक अनुकूलित है; हालांकि, बड़े पैमाने के डेटा संचालनों के भीतर इसके आवेदन को अनावश्यक प्रोसेसिंग ओवरहेड से बचने के लिए अभी भी विवेकपूर्ण होना चाहिए।

एक विकल्प पर विचार करने के लिए, विशेष रूप से जब जटिल पैटर्न के साथ काम कर रहे हों या स्थानीयकृत-विशिष्ट परिवर्तनों की आवश्यकता हो, `toLocaleLowerCase()` विधि है। यह वैरिएंट अक्षरों को निम्न मामले में परिवर्तित करने के लिए स्थानीयकृत-विशिष्ट नियमों पर विचार करता है, जो बहुभाषीय समर्थन वाले अनुप्रयोगों के लिए महत्वपूर्ण हो सकता है:

```javascript
var stringWithUmlaut = "MÄRZ";
var lowerCaseUmlaut = stringWithUmlaut.toLocaleLowerCase('de-DE');

Logger.log(lowerCaseUmlaut); // आउटपुट: märz
```

अतिरिक्त जटिलता के बावजूद, `toLocaleLowerCase()` अंतर्राष्ट्रीय अनुप्रयोगों के लिए एक शक्तिशाली उपकरण है, यह सुनिश्चित करता है कि परिवर्तन उपयोगकर्ता के स्थानीयकृत के भाषाई मानदंडों का सम्मान करता है। जो भी विधि आप चुनते हैं, Google Apps Script में स्ट्रिंग्स को लोअर केस में परिवर्तित करना पाठ संसाधन का एक मौलिक हिस्सा बना रहता है, जो उपयोगकर्ता इनपुट और मानकीकृत डेटा हैंडलिंग के बीच का अंतर पाटता है।
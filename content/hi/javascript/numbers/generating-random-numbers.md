---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-27T20:35:14.958810-07:00
model:                 gpt-4-0125-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

JavaScript में यादृच्छिक संख्याएँ उत्पन्न करना एक तकनीक है जिसका उपयोग ऐसे अनुप्रयोगों में अनिश्चितता पैदा करने के लिए किया जाता है, जैसे कि ऐसे खेल जिन्हें यादृच्छिक शत्रु व्यवहार की आवश्यकता होती है या सुरक्षा एल्गोरिदम जिन्हें क्रिप्टोग्राफिक यादृच्छिकता की आवश्यकता होती है। गतिशील उपयोगकर्ता अनुभवों और सुरक्षित अनुप्रयोगों के विकास के लिए यह क्षमता महत्वपूर्ण है।

## कैसे:

### मूल यादृच्छिक संख्या उत्पन्न करना

JavaScript में एक यादृच्छिक संख्या उत्पन्न करने का सबसे सीधा तरीका `Math.random()` का उपयोग करना है। यह फंक्शन 0 (सम्मिलित) से 1 (बहिष्कृत) की श्रेणी में एक तैरता-पॉइंट, प्रौद्योगिकी यादृच्छिक संख्या लौटाता है।

```javascript
let randomNumber = Math.random();
console.log(randomNumber);
```

### एक श्रेणी के भीतर एक यादृच्छिक संख्या उत्पन्न करना

अक्सर, आप एक विशिष्ट श्रेणी के भीतर एक यादृच्छिक पूर्णांक चाहेंगे। इसे `Math.random()` के आउटपुट को स्केलिंग और राउंडिंग करके प्राप्त किया जा सकता है।

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandomInt(1, 100));
```

### क्रिप्टोग्राफिकली सुरक्षित यादृच्छिक संख्याएं

उन अनुप्रयोगों के लिए जिन्हें यादृच्छिकता की उच्च डिग्री की आवश्यकता होती है (जैसे, क्रिप्टोग्राफिक ऑपरेशन), `crypto.getRandomValues()` मेथड का उपयोग किया जा सकता है। यह क्रिप्टोग्राफिक यादृच्छिकता प्रदान करता है, जो `Math.random()` द्वारा उत्पन्न प्रौद्योगिकी यादृच्छिक संख्याओं से अलग है।

```javascript
(function generateSecureRandom() {
  let array = new Uint32Array(1);
  window.crypto.getRandomValues(array);
  console.log(array[0]);
})();
```

## गहराई में

ऐतिहासिक रूप से, JavaScript में यादृच्छिक संख्या उत्पादन केवल `Math.random()` फंक्शन पर निर्भर करता था। जबकि अधिकांश आम उपयोगों के लिए यह सुविधाजनक है, इसका एल्गोरिथम, आमतौर पर मर्सेन ट्विस्टर जैसे प्रौद्योगिकी यादृच्छिक संख्या जेनरेटर (PRNG) का एक प्रकार, क्रिप्टोग्राफिक सुरक्षा प्रदान नहीं करता है।

Web Cryptography API के परिचय ने `crypto.getRandomValues()` मेथड पेश किया, जो सुरक्षा-संवेदनशील अनुप्रयोगों के लिए उपयुक्त कम पूर्वानुमान योग्य संख्याएँ उत्पन्न करने का एक तरीका प्रदान करता है। यह मेथड अंतर्निहित ऑपरेटिंग सिस्टम के यादृच्छिकता स्रोतों में टैप करता है, जैसे कि Unix/Linux पर `/dev/random`, जो क्रिप्टोग्राफिक ऑपरेशनों के लिए अधिक विश्वसनीय और उपयुक्त हैं।

जिस कार्य के लिए आप हाथ लगाएं उसके लिए सही मेथड चुनना महत्वपूर्ण है। `Math.random()` सरल खेलों, एनिमेशनों, या किसी भी मामले के लिए पर्याप्त है जहाँ यादृच्छिकता की गुणवत्ता महत्वपूर्ण नहीं है। हालांकि, सुरक्षा सुविधाओं के लिए, जैसे कि पासवर्ड रीसेट टोकन या कोई क्रिप्टोग्राफिक ऑपरेशन, `crypto.getRandomValues()` इसकी श्रेष्ठ यादृच्छिकता गुणवत्ता के कारण बेहतर विकल्प है।

विशेष रूप से, `Math.random()` अधिकांश कार्यान्वयनों में एक ज्ञात पूर्वाग्रह के साथ संख्याएँ उत्पन्न करता है, अर्थात कुछ संख्याएँ दूसरों की तुलना में अधिक संभावना से होती हैं। यद्यपि यह पूर्वाग्रह न्यूनतम है और आम तौर पर सामान्य अनुप्रयोगों के लिए अप्रत्यक्ष होता है, यह `Math.random()` को किसी भी क्रिप्टोग्राफिक संदर्भ या निष्पक्षता महत्वपूर्ण हो, जैसे कि ऑनलाइन जुआ, में उपयोग किए जाने से अयोग्य बनाता है।

निष्कर्ष में, जबकि JavaScript के निर्मित फ़ंक्शन यादृच्छिक संख्याओं को उत्पन्न करने के लिए व्यापक श्रेणी की आवश्यकताओं को कवर करते हैं, प्रत्येक मेथड के अंतर और सीमाओं को समझना उनके उपयुक्त अनुप्रयोग के लिए अनिवार्य है।
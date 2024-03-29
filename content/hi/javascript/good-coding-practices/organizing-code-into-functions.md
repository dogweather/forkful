---
date: 2024-01-26 01:11:12.647052-07:00
description: "\u0915\u094B\u0921 \u0915\u094B \u0915\u093E\u0930\u094D\u092F\u094B\
  \u0902 \u092E\u0947\u0902 \u0935\u094D\u092F\u0935\u0938\u094D\u0925\u093F\u0924\
  \ \u0930\u0916\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0938\u092E\u093E\u0930\
  \u094B\u0939\u094B\u0902 (functions) \u092E\u0947\u0902 \u092C\u093E\u0902\u091F\
  \u0928\u093E \u092A\u0941\u0928: \u092A\u094D\u0930\u092F\u094B\u091C\u094D\u092F\
  \ \u0916\u0902\u0921\u094B\u0902 \u092E\u0947\u0902 \u0915\u093E\u0930\u094D\u092F\
  \ \u0915\u0930\u0924\u093E \u0939\u0948, \u091C\u094B \u0915\u094B\u0921 \u0915\u094B\
  \ \u0938\u093E\u092B \u0914\u0930 \u0905\u0927\u093F\u0915 \u0930\u0916\u0930\u0916\
  \u093E\u0935 \u0938\u093E\u091D\u0947 \u0915\u0930\u0928\u0947\u2026"
lastmod: '2024-03-13T22:44:52.999067-06:00'
model: gpt-4-1106-preview
summary: "\u0915\u094B\u0921 \u0915\u094B \u0915\u093E\u0930\u094D\u092F\u094B\u0902\
  \ \u092E\u0947\u0902 \u0935\u094D\u092F\u0935\u0938\u094D\u0925\u093F\u0924 \u0930\
  \u0916\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0938\u092E\u093E\u0930\u094B\
  \u0939\u094B\u0902 (functions) \u092E\u0947\u0902 \u092C\u093E\u0902\u091F\u0928\
  \u093E \u092A\u0941\u0928: \u092A\u094D\u0930\u092F\u094B\u091C\u094D\u092F \u0916\
  \u0902\u0921\u094B\u0902 \u092E\u0947\u0902 \u0915\u093E\u0930\u094D\u092F \u0915\
  \u0930\u0924\u093E \u0939\u0948, \u091C\u094B \u0915\u094B\u0921 \u0915\u094B \u0938\
  \u093E\u092B \u0914\u0930 \u0905\u0927\u093F\u0915 \u0930\u0916\u0930\u0916\u093E\
  \u0935 \u0938\u093E\u091D\u0947 \u0915\u0930\u0928\u0947\u2026"
title: "\u0915\u094B\u0921 \u0915\u094B \u092B\u0902\u0915\u094D\u0936\u0928\u094D\
  \u0938 \u092E\u0947\u0902 \u0935\u094D\u092F\u0935\u0938\u094D\u0925\u093F\u0924\
  \ \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
कोड को कार्यों में व्यवस्थित रखने के लिए समारोहों (functions) में बांटना पुन: प्रयोज्य खंडों में कार्य करता है, जो कोड को साफ और अधिक रखरखाव साझे करने योग्य बनाता है। हम यह अतिरिक्तता को कम करने, परीक्षण को आसान बनाने और पठनीयता में सुधार करने के लिए करते हैं।

## कैसे करें:

```javascript
// एक समारोह की परिभाषा दें जो एक आयत का क्षेत्रफल गणना करे
function calculateArea(width, height) {
  return width * height;
}

// समारोह को कॉल करें और परिणाम प्रिंट करें
let area = calculateArea(5, 3);
console.log(area); // आउटपुट: 15
```

```javascript
// समारोह उपयोग करके संबंधित कार्यक्षमता को समूहित करें
function greet(name) {
  console.log(`Hello, ${name}!`);
}

function farewell(name) {
  console.log(`Goodbye, ${name}!`);
}

greet('Alice'); // आउटपुट: Hello, Alice!
farewell('Bob'); // आउटपुट: Goodbye, Bob!
```

## गहराई से जानकारी
ऐतिहासिक रूप से, प्रारंभिक संस्करणों के BASIC या Assembly जैसे आदेशिक प्रोग्रामिंग भाषाओं में समारोहों द्वारा प्रदान की गई अमूर्तीकरण की कमी थी। समय के साथ, C जैसी भाषाओं में मॉड्यूलर कोड की अवधारणा ने यह विचार पेश किया कि कोड को इकाइयों (समारोहों या प्रक्रियाओं) में तोड़ने से बेहतर संगठन और स्पष्ट तर्क मिलता है।

JavaScript में, सादे समारोहों के अलावा, हमारे पास ES6 (2015) से एरो समारोह हैं, जो एक संक्षिप्त सिंटैक्स प्रदान करते हैं और गैर-मेथड समारोहों के लिए उपयुक्त हैं।

JavaScript में कोड व्यवस्थित करने के आसपास के विकल्प और संवर्द्धन में वर्गों (classes) का उपयोग करते हुए वस्तु-उन्मुख दृष्टिकोण या फ़ंक्शनल प्रोग्रामिंग पैराडाइम्स शामिल हैं जो समारोहों को प्रथम श्रेणी के नागरिकों के रूप में मानते हैं।

कार्यान्वयन की दृष्टि से, JavaScript समारोह समापनों (closures) का समर्थन करते हैं, जो कार्यान्वयन के बाद एक समारोह के स्कोप तक पहुंच को बनाए रखने का एक तरीका प्रदान करता है, जो एनकैप्सुलेशन के लिए और कारखाने के समारोहों बनाने के लिए, दूसरे पैटर्नों के बीच में शक्तिशाली है।

## देखें भी
- MDN वेब डॉक्स पर समारोह: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Functions
- JavaScript डिज़ाइन पैटर्न्स: https://addyosmani.com/resources/essentialjsdesignpatterns/book/
- साफ कोड JavaScript: https://github.com/ryanmcdermott/clean-code-javascript

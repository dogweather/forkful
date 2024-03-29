---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:09:08.702056-07:00
description: "\u091C\u091F\u093F\u0932 \u0938\u0902\u0916\u094D\u092F\u093E\u090F\u0901\
  , \u091C\u094B \u0935\u093E\u0938\u094D\u0924\u0935\u093F\u0915 \u0914\u0930 \u0915\
  \u093E\u0932\u094D\u092A\u0928\u093F\u0915 \u0907\u0915\u093E\u0907\u092F\u094B\u0902\
  \ (\u0909\u0926\u093E\u0939\u0930\u0923 \u0915\u0947 \u0932\u093F\u090F, 3 + 4i)\
  \ \u0915\u0947 \u0938\u0902\u092F\u094B\u091C\u0928 \u0915\u0947 \u0930\u0942\u092A\
  \ \u092E\u0947\u0902 \u092A\u094D\u0930\u0938\u094D\u0924\u0941\u0924 \u0915\u0940\
  \ \u091C\u093E\u0924\u0940 \u0939\u0948\u0902, \u0935\u093F\u092D\u093F\u0928\u094D\
  \u0928 \u0917\u0923\u0928\u093E \u0938\u092E\u0938\u094D\u092F\u093E\u0913\u0902\
  \ \u092E\u0947\u0902 \u092E\u094C\u0932\u093F\u0915 \u0939\u094B\u0924\u0940\u2026"
lastmod: '2024-03-13T22:44:51.501136-06:00'
model: gpt-4-0125-preview
summary: "\u091C\u091F\u093F\u0932 \u0938\u0902\u0916\u094D\u092F\u093E\u090F\u0901\
  , \u091C\u094B \u0935\u093E\u0938\u094D\u0924\u0935\u093F\u0915 \u0914\u0930 \u0915\
  \u093E\u0932\u094D\u092A\u0928\u093F\u0915 \u0907\u0915\u093E\u0907\u092F\u094B\u0902\
  \ (\u0909\u0926\u093E\u0939\u0930\u0923 \u0915\u0947 \u0932\u093F\u090F, 3 + 4i)\
  \ \u0915\u0947 \u0938\u0902\u092F\u094B\u091C\u0928 \u0915\u0947 \u0930\u0942\u092A\
  \ \u092E\u0947\u0902 \u092A\u094D\u0930\u0938\u094D\u0924\u0941\u0924 \u0915\u0940\
  \ \u091C\u093E\u0924\u0940 \u0939\u0948\u0902, \u0935\u093F\u092D\u093F\u0928\u094D\
  \u0928 \u0917\u0923\u0928\u093E \u0938\u092E\u0938\u094D\u092F\u093E\u0913\u0902\
  \ \u092E\u0947\u0902 \u092E\u094C\u0932\u093F\u0915 \u0939\u094B\u0924\u0940\u2026"
title: "\u091C\u091F\u093F\u0932 \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902\
  \ \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
जटिल संख्याएँ, जो वास्तविक और काल्पनिक इकाइयों (उदाहरण के लिए, 3 + 4i) के संयोजन के रूप में प्रस्तुत की जाती हैं, विभिन्न गणना समस्याओं में मौलिक होती हैं, विशेष रूप से इंजीनियरिंग, भौतिकी, और प्रयुक्त गणित में। Google Apps Script में इन संख्याओं को संभालना सीखना प्रोग्रामरों को वैज्ञानिक गणना, सिग्नल प्रोसेसिंग और उससे आगे तक अपनी क्षमता को विस्तृत करने की अनुमति देता है।

## कैसे करें:
Google Apps Script में जटिल संख्याओं के लिए बिल्ट-इन सपोर्ट नहीं होती है, जिसके कारण अनुकूलित कार्यक्षमता के कार्यान्वयन की आवश्यकता होती है। नीचे जटिल संख्याओं को संभालने के लिए एक मूल संरचना दी गई है, जिसमें जोड़, घटाव, और गुणा शामिल है।

```javascript
// जटिल संख्याओं के लिए एक निर्माता विधि परिभाषित करें
function Complex(real, imag) {
  this.real = real;
  this.imag = imag;
}

// दो जटिल संख्याओं को जोड़ने के लिए विधि
Complex.prototype.add = function(other) {
  return new Complex(this.real + other.real, this.imag + other.imag);
};

// दो जटिल संख्याओं को घटाने के लिए विधि
Complex.prototype.subtract = function(other) {
  return new Complex(this.real - other.real, this.imag - other.imag);
};

// दो जटिल संख्याओं को गुणा करने के विधि
Complex.prototype.multiply = function(other) {
  return new Complex(
    this.real * other.real - this.imag * other.imag,
    this.real * other.imag + this.imag * other.real
  );
};

// उदाहरण का उपयोग
var num1 = new Complex(3, 4);
var num2 = new Complex(1, 2);

// दो जटिल संख्याओं को जोड़ें
var sum = num1.add(num2);
console.log(`सुम: ${sum.real} + ${sum.imag}i`); // सुम: 4 + 6i

// दो जटिल संख्याओं को घटाएं
var difference = num1.subtract(num2);
console.log(`अंतर: ${difference.real} + ${difference.imag}i`); // अंतर: 2 + 2i

// दो जटिल संख्याओं को गुणा करें
var product = num1.multiply(num2);
console.log(`उत्पाद: ${product.real} + ${product.imag}i`); // उत्पाद: -5 + 10i
```

## विस्तृत अध्ययन:
जटिल संख्याओं की अवधारणा 16वीं शताब्दी में वापस चली जाती है, लेकिन यह गणितज्ञों जैसे कि यूलर और गौस के काम थे जिन्होंने उन्हें गणित में एक स्थान दिलाया। उनकी उपयोगिता के बावजूद, जटिल संख्याएँ JavaScript या Google Apps Script में सीधे समर्थित नहीं हैं। नेटिव सपोर्ट की कमी का मतलब है कि जटिल संख्याओं पर ऑपरेशन्स को मैन्युअली लागू करना पड़ता है, जैसा कि दिखाया गया है। हालांकि यह एक अच्छा सीखने का मौका प्रदान करता है और बुनियादी जरूरतों के लिए पर्याप्त कार्यक्षमता देता है, जटिल संख्याओं की आवश्यकता वाले भारी गणनात्मक काम के लिए, एक गणितीय गणना के लिए अधिक उपयुक्त प्रोग्रामिंग पर्यावरणों का उपयोग करने पर विचार कर सकता है, जैसे कि Python नुमPy के साथ, जो जटिल संख्याओं को संभालने के लिए बिल्ट-इन, उच्च अनुकूलित ऑपरेशन्स प्रदान करता है। फिर भी, Google Apps Script में मूलभूत ऑपरेशन्स को समझना और लागू करना उन लोगों के लिए एक उपयोगी अभ्यास है जो अपने प्रोग्रामिंग कौशल को व्यापक रूप से समझना चाहते हैं और उन्हें विभिन्न संदर्भों में लागू करना चाहते हैं।

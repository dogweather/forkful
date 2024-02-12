---
title:                "जटिल संख्याओं के साथ काम करना"
aliases: - /hi/javascript/working-with-complex-numbers.md
date:                  2024-01-26T04:44:55.907875-07:00
model:                 gpt-4-0125-preview
simple_title:         "जटिल संख्याओं के साथ काम करना"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
जटिल संख्या वे संख्या होती हैं जिनमें एक वास्तविक और एक काल्पनिक भाग होता है (जैसे कि 3 + 4i)। वे विभिन्न प्रोग्रामिंग समस्याओं में, खासकर सिग्नल प्रोसेसिंग, क्वांटम कंप्यूटिंग, और बहुपदीय समीकरणों को हल करने में उभरते हैं। प्रोग्रामर इन तरह के कार्यों को प्रभावी ढंग से संभालने के लिए उनके साथ जूझते हैं।

## कैसे:
जावास्क्रिप्ट में जटिल संख्याओं के लिए निर्मित समर्थन नहीं है, परंतु आप ऑब्जेक्ट्स और गणित के साथ इसे संभाल सकते हैं। यहाँ एक त्वरित नज़र है।

```javascript
class ComplexNumber {
  constructor(real, imaginary) {
    this.real = real;
    this.imaginary = imaginary;
  }

  add(other) {
    return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
  }

  // ...ज़रुरत के अनुसार और अधिक विधियाँ (घटाना, गुणा, भाग) जोड़ें

  toString() {
    return `${this.real} + ${this.imaginary}i`;
  }
}

const a = new ComplexNumber(1, 2);
const b = new ComplexNumber(3, 4);
const result = a.add(b);

console.log(`परिणाम: ${result}`); // परिणाम: 4 + 6i
```

## गहराई से विचार
जटिल संख्याएँ 16वीं शताब्दी से हैं, इटालियन गणितज्ञ गेरोलामो कार्डानो की बदौलत। वे विभिन्न क्षेत्रों में, जैसे कि इंजीनियरिंग और भौतिकी में महत्वपूर्ण बन गए। आधुनिक प्रोग्रामिंग में, वे सिमुलेशन और बहु-आयामीता की आवश्यकता वाले एल्गोरिदमों के लिए कुंजी हैं।

अब, जावास्क्रिप्ट मूल रूप से जटिल संख्याओं के लिए तैयार नहीं है। परन्तु DIY विकल्प के अलावा, आप math.js या numeric.js जैसे गणितीय पुस्तकालयों का उपयोग कर सकते हैं। वे भारी जटिल संख्या उठाने की शक्ति प्रदान करते हैं, अधिक क्रियाओं, परिमाण की गणना, और तर्क खोजने जैसे लाभ जोड़ते हैं।

अंतर्निहित रूप से, जब आप जटिल संख्याओं के साथ कार्य करते हैं, तो यह दो अलग-अलग संख्याओं को कूल्हों पर बाँधकर प्रबंधन करने जैसा है। जोड़ और घटाव सीधे खेल हैं—वास्तविक को वास्तविक के साथ, काल्पनिक को काल्पनिक के साथ मिलाएं। गुणा और भाग लेने में क्रॉस-टर्म नृत्य के साथ मसालेदार हो जाते हैं और अधिक ध्यान की आवश्यकता होती है।

## यह भी देखें
- JavaScript पर MDN वेब डॉक्स: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
- Math.js, जटिल संख्याएँ सहित एक गणित पुस्तकालय: https://mathjs.org/docs/datatypes/complex_numbers.html
- Numeric.js, एक और पुस्तकालय: http://numericjs.com/documentation.html
- जटिल संख्याओं पर गहराई से डाइव (गणित केंद्रित): https://mathworld.wolfram.com/ComplexNumber.html

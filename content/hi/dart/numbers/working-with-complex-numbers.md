---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:08.079253-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u0921\u093E\u0930\
  \u094D\u091F \u0938\u094D\u0935\u092F\u0902 \u092E\u0947\u0902 \u091C\u091F\u093F\
  \u0932 \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u0947 \u0932\u093F\
  \u090F \u090F\u0915 \u0907\u0928-\u092C\u093F\u0932\u094D\u091F \u0932\u093E\u0907\
  \u092C\u094D\u0930\u0947\u0930\u0940 \u0936\u093E\u092E\u093F\u0932 \u0928\u0939\
  \u0940\u0902 \u0915\u0930\u0924\u093E, \u091C\u093F\u0938\u0915\u0940 \u0906\u0935\
  \u0936\u094D\u092F\u0915\u0924\u093E \u092F\u093E \u0924\u094B \u090F\u0915 \u0915\
  \u0938\u094D\u091F\u092E \u091C\u091F\u093F\u0932 \u0938\u0902\u0916\u094D\u092F\
  \u093E \u0915\u094D\u0932\u093E\u0938 \u0915\u0947 \u0915\u093E\u0930\u094D\u092F\
  \u093E\u0928\u094D\u0935\u092F\u0928 \u092F\u093E\u2026"
lastmod: '2024-04-05T21:53:53.810441-06:00'
model: gpt-4-0125-preview
summary: "\u0921\u093E\u0930\u094D\u091F \u0938\u094D\u0935\u092F\u0902 \u092E\u0947\
  \u0902 \u091C\u091F\u093F\u0932 \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902\
  \ \u0915\u0947 \u0932\u093F\u090F \u090F\u0915 \u0907\u0928-\u092C\u093F\u0932\u094D\
  \u091F \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0936\u093E\u092E\
  \u093F\u0932 \u0928\u0939\u0940\u0902 \u0915\u0930\u0924\u093E, \u091C\u093F\u0938\
  \u0915\u0940 \u0906\u0935\u0936\u094D\u092F\u0915\u0924\u093E \u092F\u093E \u0924\
  \u094B \u090F\u0915 \u0915\u0938\u094D\u091F\u092E \u091C\u091F\u093F\u0932 \u0938\
  \u0902\u0916\u094D\u092F\u093E \u0915\u094D\u0932\u093E\u0938 \u0915\u0947 \u0915\
  \u093E\u0930\u094D\u092F\u093E\u0928\u094D\u0935\u092F\u0928 \u092F\u093E \u0915\
  \u093F\u0938\u0940 \u0924\u0943\u0924\u0940\u092F-\u092A\u0915\u094D\u0937 \u0932\
  \u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u0947 \u0909\u092A\u092F\
  \u094B\u0917 \u0915\u094B \u0909\u0924\u094D\u092A\u0928\u094D\u0928 \u0915\u0930\
  \u0924\u0940 \u0939\u0948\u0964 \u0935\u0948\u091C\u094D\u091E\u093E\u0928\u093F\
  \u0915 \u0915\u0902\u092A\u094D\u092F\u0942\u091F\u093F\u0902\u0917 \u0915\u093E\
  \u0930\u094D\u092F\u094B\u0902 \u0915\u0947 \u0932\u093F\u090F \u090F\u0915 \u0932\
  \u094B\u0915\u092A\u094D\u0930\u093F\u092F \u0935\u093F\u0915\u0932\u094D\u092A\
  , \u091C\u093F\u0938\u092E\u0947\u0902 \u091C\u091F\u093F\u0932 \u0938\u0902\u0916\
  \u094D\u092F\u093E\u0913\u0902 \u0915\u0947 \u0932\u093F\u090F \u0938\u092E\u0930\
  \u094D\u0925\u0928 \u0936\u093E\u092E\u093F\u0932 \u0939\u0948, `package:scidart`\
  \ \u0939\u0948\u0964."
title: "\u091C\u091F\u093F\u0932 \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902\
  \ \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 14
---

## कैसे करें:
डार्ट स्वयं में जटिल संख्याओं के लिए एक इन-बिल्ट लाइब्रेरी शामिल नहीं करता, जिसकी आवश्यकता या तो एक कस्टम जटिल संख्या क्लास के कार्यान्वयन या किसी तृतीय-पक्ष लाइब्रेरी के उपयोग को उत्पन्न करती है। वैज्ञानिक कंप्यूटिंग कार्यों के लिए एक लोकप्रिय विकल्प, जिसमें जटिल संख्याओं के लिए समर्थन शामिल है, `package:scidart` है।

### एक बुनियादी जटिल संख्या क्लास कार्यान्वित करना
सरल कार्यों के लिए, आप आसानी से अपना स्वयं का जटिल संख्या क्लास परिभाषित कर सकते हैं:

```dart
class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  // दो जटिल संख्याओं का योग
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // आसान डिबगिंग के लिए स्ट्रिंग प्रतिनिधित्व
  @override
  String toString() => '${real} + ${imaginary}i';
}

void main() {
  var number1 = Complex(3, 4);
  var number2 = Complex(1, 2);

  var sum = number1 + number2;
  print(sum);  // 4.0 + 6.0i
}
```

### उन्नत क्रियाओं के लिए SciDart का उपयोग
अधिक जटिल क्रियाओं के लिए या जब प्रदर्शन महत्वपूर्ण हो, `package:scidart` अन्य वैज्ञानिक कंप्यूटिंग कार्यक्षमताओं के साथ जटिल संख्याओं के लिए व्यापक समर्थन प्रदान करता है। पहले, आपके pubspec.yaml में SciDart जोड़ें:

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

यहाँ जटिल संख्याओं के साथ बुनियादी क्रियाएँ करने का तरीका है जो SciDart का उपयोग करता है:

```dart
import 'package:scidart/numdart.dart';

void main() {
  // जटिल संख्याएँ बनाना
  var complexNum1 = Complex(real: 5, imaginary: 3);
  var complexNum2 = Complex(real: 2, imaginary: 7);

  // योग
  var sum = complexAdd(complexNum1, complexNum2);
  
  // गुणन
  var product = complexMultiply(complexNum1, complexNum2);

  print('Sum: ${sum.toString()}');  // Sum: Complex(real: 7.0, imaginary: 10.0)
  print('Product: ${product.toString()}');  // Product: Complex(real: -11.0, imaginary: 41.0)
}
```

ये उदाहरण डार्ट में जटिल संख्याओं के बुनियादी हेरफेर और उपयोग को दर्शाते हैं, दोनों कस्टम कार्यान्वयन के माध्यम से और SciDart पुस्तकालय के माध्यम से, वैज्ञानिक कंप्यूटिंग कार्यों के लिए डार्ट की लचीलेपन और शक्ति को हाइलाइट करते हैं।

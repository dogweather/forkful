---
title:                "जटिल संख्याओं के साथ काम करना"
date:                  2024-03-08T21:58:08.079253-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

जटिल संख्याएँ, जिनमें एक वास्तविक और एक काल्पनिक भाग होता है (आमतौर पर a + bi के रूप में दर्शाया जाता है), आयामहीन संख्याओं की अवधारणा को दो-आयामी स्थान तक विस्तारित करती हैं। प्रोग्रामर इलेक्ट्रिकल इंजीनियरिंग, क्वांटम कंप्यूटिंग और फ्लूइड डायनामिक्स जैसे क्षेत्रों में जटिल संख्याओं के साथ काम करते हैं ताकि उन परिघटनाओं का मॉडल तैयार किया जा सके जिन्हें केवल वास्तविक संख्याओं के एकल आयाम के साथ प्रस्तुत नहीं किया जा सकता।

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

---
date: 2024-01-26 04:46:40.832280-07:00
description: "\u091C\u091F\u093F\u0932 \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902\
  \ \u092E\u0947\u0902 \u090F\u0915 \u0935\u093E\u0938\u094D\u0924\u0935\u093F\u0915\
  \ \u092D\u093E\u0917 \u0914\u0930 \u090F\u0915 \u0915\u093E\u0932\u094D\u092A\u0928\
  \u093F\u0915 \u092D\u093E\u0917 \u0939\u094B\u0924\u093E \u0939\u0948 \u0914\u0930\
  \ \u0935\u0947 \u0935\u093F\u092D\u093F\u0928\u094D\u0928 \u0915\u094D\u0937\u0947\
  \u0924\u094D\u0930\u094B\u0902 \u091C\u0948\u0938\u0947 \u0907\u0902\u091C\u0940\
  \u0928\u093F\u092F\u0930\u093F\u0902\u0917, \u092D\u094C\u0924\u093F\u0915\u0940\
  , \u0914\u0930 \u0915\u0902\u092A\u094D\u092F\u0942\u091F\u0930 \u0917\u094D\u0930\
  \u093E\u092B\u093F\u0915\u094D\u0938 \u092E\u0947\u0902 \u092E\u0939\u0924\u094D\
  \u0935\u092A\u0942\u0930\u094D\u0923 \u0939\u0948\u0902\u0964\u2026"
lastmod: '2024-03-13T22:44:51.951020-06:00'
model: gpt-4-0125-preview
summary: "\u091C\u091F\u093F\u0932 \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902\
  \ \u092E\u0947\u0902 \u090F\u0915 \u0935\u093E\u0938\u094D\u0924\u0935\u093F\u0915\
  \ \u092D\u093E\u0917 \u0914\u0930 \u090F\u0915 \u0915\u093E\u0932\u094D\u092A\u0928\
  \u093F\u0915 \u092D\u093E\u0917 \u0939\u094B\u0924\u093E \u0939\u0948 \u0914\u0930\
  \ \u0935\u0947 \u0935\u093F\u092D\u093F\u0928\u094D\u0928 \u0915\u094D\u0937\u0947\
  \u0924\u094D\u0930\u094B\u0902 \u091C\u0948\u0938\u0947 \u0907\u0902\u091C\u0940\
  \u0928\u093F\u092F\u0930\u093F\u0902\u0917, \u092D\u094C\u0924\u093F\u0915\u0940\
  , \u0914\u0930 \u0915\u0902\u092A\u094D\u092F\u0942\u091F\u0930 \u0917\u094D\u0930\
  \u093E\u092B\u093F\u0915\u094D\u0938 \u092E\u0947\u0902 \u092E\u0939\u0924\u094D\
  \u0935\u092A\u0942\u0930\u094D\u0923 \u0939\u0948\u0902\u0964\u2026"
title: "\u091C\u091F\u093F\u0932 \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902\
  \ \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
जटिल संख्याओं में एक वास्तविक भाग और एक काल्पनिक भाग होता है और वे विभिन्न क्षेत्रों जैसे इंजीनियरिंग, भौतिकी, और कंप्यूटर ग्राफिक्स में महत्वपूर्ण हैं। प्रोग्रामर उन्हें ऐसी समस्याओं को हल करने के लिए उपयोग करते हैं जिन्हें सामान्य वास्तविक संख्याएं संभाल नहीं सकतीं।

## कैसे:
रस्ट में जटिल संख्याओं का समर्थन निर्मित नहीं है, लेकिन `num-complex` जैसे क्रेट्स आपकी मदद के लिए हैं। इसका उपयोग कैसे करें यह यहां है:

```rust
use num_complex::Complex;

fn main() {
    let a = Complex::new(2.0, 3.0); // 2 + 3i
    let b = Complex::new(1.0, -4.0); // 1 - 4i

    let sum = a + b;
    let product = a * b;

    println!("Sum: {}", sum); // योग: 3 - 1i
    println!("Product: {}", product); // उत्पादन: 14 - 5i
}
```
इस जादू को साकार करने के लिए आपको `num_complex` को अपनी `Cargo.toml` में जोड़ना होगा।

## गहन परीक्षण
जटिल संख्याएं 16वीं शताब्दी में उत्पन्न हुई थीं, लेकिन वास्तव में 18वीं शताब्दी में तब उठीं जब गणितज्ञों जैसे कि यूलर ने उनके साथ खेलना शुरू किया।

नेटिव जटिल संख्या ऑपरेशन्स के बिना, रस्ट जैसी भाषाएं तृतीय-पक्ष पुस्तकालयों पर निर्भर करती हैं। `num-complex` ऐसा ही एक क्रेट है और `num` क्रेट संग्रह का हिस्सा है जो रस्ट के लिए संख्यात्मक प्रकारों और गुणों को प्रदान करने का उद्देश्य रखता है।

यह उल्लेख करने योग्य है कि कुछ भाषाएं (जैसे कि पाइथन) में जटिल संख्याओं के लिए निर्मित समर्थन है, जबकि अन्य (जैसे कि C++, `<complex>` हेडर के साथ) उन्हें मानक पुस्तकालय का हिस्सा के रूप में प्रदान करती हैं। रस्ट में, मानक पुस्तकालय को छोटा रखने के निर्णय का मतलब है कि आप अक्सर अतिरिक्त कार्यक्षमता के लिए सामुदायिक-निर्मित क्रेट्स की ओर देखेंगे।

## यह भी देखें
- [रस्ट पुस्तक](https://doc.rust-lang.org/book/): रस्ट के बारे में और अधिक जानने और बाहरी क्रेट्स के साथ कैसे काम करें इसके लिए।
- [जटिल संख्या विकिपीडिया](https://en.wikipedia.org/wiki/Complex_number): जटिल संख्याओं की गहन समझ के लिए।

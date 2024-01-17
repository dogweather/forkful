---
title:                "स्टैंडर्ड एरर में लिखना"
html_title:           "Arduino: स्टैंडर्ड एरर में लिखना"
simple_title:         "स्टैंडर्ड एरर में लिखना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Arduino में सामान्य त्रुटि में लिखने क्या हैं और क्यों किया जाता हैं?

## What &amp; Why?
सामान्य त्रुटि में लिखना यह है कि जब हम अपने प्रोग्राम के दौरान त्रुटियां मिलती हैं तो हम उन्हें कहीं न कहीं सुरक्षित रूप से दर्ज कर सकते हैं। प्रोग्रामर अपने कोड में त्रुटियों का पता लगाकर उन्हें सही कर सकते हैं और अपने प्रोग्राम को बेहतर बना सकते हैं।

## How to:
जब हम कोई त्रुटि प्रोग्राम चलाते हैं तो हमें थोड़ा सा कोड जोड़ना होता हैं। यहां नीचे कुछ कोड दिया है, जिसे आप अपने एआरडीयूइनो में उपयोग कर सकते हैं। इससे कोड के दौरान सामान्य त्रुटि दिखाई देगी।

```Arduino
 Serial.println("This will print to standard output");
 Serial.println("This will print to standard error", STDERR);
```

## Deep Dive:
सामान्य त्रुटि में लिखने की अन्य विकल्प भी है, जैसे कि Serial.print या उपयोगकर्ता द्वारा निर्दिष्ट त्रुटि कोड। आपको अपने के अनुसार सर्वोत्तम विकल्प चुनना होगा। सामान्य त्रुटि में लिखना पुराने मस्टर्डाउन स्टैंडर्ड लाइब्रेरी से लाए गए है, जो अब डीपीआर के रूप में जाना जाता है।

## See Also:
आप अधिक जानकारी के लिए इन लिंक्स पर जा सकते हैं।
- हिटवार्ड ट्रिपलेट्स (https://hindit.itp.ac.in/~penderya/C/P-P-HCT/lib/stderr.html)
- एस के ग्राहक फोरम (https://forum.arduino.cc/index.php?topic=68092.0)
- सामान्य त्रुटि मॉड्यूल आदेश (https://sourceforge.net/projects/stderr-ard/)
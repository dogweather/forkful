---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

"उपस्ट्रिंग निकालना" एक मूल स्ट्रिंग से छोटी स्ट्रिंग को "निकालना" है। प्रोग्रामर्स इसे तब करते हैं जब उन्हें स्ट्रिंग के एक विशेष हिस्से की आवश्यकता होती है, जैसे कि उपयोगकर्ता के नाम के पहले 5 अक्षरों, ईमेल आईडी का डोमेन नाम आदि।

## कैसे करें:

```typescript
let str: string = "Hello, World!";
let subStr: string = str.substring(0, 5); // "Hello"
console.log(subStr);  // Outputs: "Hello"
```

```typescript
let email: string = "example@domain.com";
let domain: string = email.substring(email.indexOf('@') + 1);
console.log(domain);  // Outputs: "domain.com"
```

## गहराई से समझे:

उपस्ट्रिंग निकालने के संदर्भ में कुछ महत्वपूर्ण बातें हैं:

1. ऐतिहासिक संदर्भ: उपस्ट्रिंग निकालने की क्षमता का उद्गम कंप्यूटर प्रोग्रामिंग के सबसे पुराने दिनों से ही कर रहा है, जब ASCII स्ट्रिंग्स का उपयोग मानव-संगठित जानकारी को मशीन पठनीय फ़ॉर्म में बदलने के लिए किया जाता था।

2. विकल्प: TypeScript आपको 'slice' नामक एक अतिरिक्त विधि भी देता है, जिसे उपस्ट्रिंग निकालने के लिए उपयोग किया जा सकता है, जैसे 'substring' का उपयोग करते हैं।

3. कार्यान्वयन विवरण: 'substring' और 'slice' विधियों के बीच मूल अंतर यह है कि 'slice' विधि नकारात्मक इंडेक्स ले सकती है, जो मूल स्ट्रिंग के अंत से शुरू होते हैं। 

## अधिक देखें:

- [TypeScript ट्यूटोरियल उप-स्ट्रिंग](https://www.javatpoint.com/typescript-string-substr)
- [JavaScript और TypeScript में स्ट्रिंग्स (mdn web docs)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
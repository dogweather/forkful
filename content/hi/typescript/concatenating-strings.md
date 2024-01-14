---
title:                "TypeScript: स्ट्रिंग्स की संयोजन"
simple_title:         "स्ट्रिंग्स की संयोजन"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप वास्तविकता में स्ट्रिंग्स को साथ जोड़ना चाहते हैं? यह वास्तव में बहुत उपयोगी हो सकता है। स्ट्रिंग विधाओं का प्रयोग आपको अपने कोड को संगठित और उपयोगी बनाने में मदद कर सकता है। हालांकि, यह शायद आपको बहुत विचित्र लगे, लेकिन आप अपने कोड को दीर्घकालिक रूप से सुरक्षित रखने के लिए इस्तेमाल कर सकते हो।

## कैसे करना है

```TypeScript
let language = "TypeScript";
let statement = "I love using ";
let output = statement + language;
console.log(output);
```

उपरोक्त कोड अपने स्क्रिप्ट को इस तरह से प्रिंट करेगा:

```TypeScript
I love using TypeScript
```

## गहरी खोज

जब आप `+` ऑपरेटर का उपयोग स्ट्रिंग्स को जोड़ने के लिए करते हैं, यह इसके पीछे आगे जाता है - स्ट्रिंग्स को जोड़ते समय, उनके बीच एक स्पेसिफिक बर्तन जोड़ा जाता है, जो आपको देखने को नहीं मिलता है। यह बर्तन रिपोर्ट कोड में से निकालने की आवश्यकता को कई चांस देता है। एक और उपाय स्ट्रिंग्स को जोड़ने के लिए `concat()` विधाओं का उपयोग करना है, जो आपको एक फ़िल्टर निकालने की सुविधा देता है। आप यह कोड देख सकते हैं:

```TypeScript
let language = "TypeScript";
let statement = "I love using ";
let output = statement.concat(language);
console.log(output);
```

अब भी आपको इसी परिणाम को प्रिंट करेगा:

```TypeScript
I love using TypeScript
```

## भी देखें

- [TypeScript Strings](https://www.typescriptlang.org/docs/handbook/strings.html)
- [JavaScript String concat() Method](https://www.w3schools.com/jsref/jsref_concat_string.asp)
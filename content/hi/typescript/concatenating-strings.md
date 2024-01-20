---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वाक्यांशों को जोड़ने का अर्थ है, कई छोटे वाक्यांशों को एक बड़े वाक्यांश में बदलने की प्रक्रिया। प्रोग्रामर इसे मानव-यंत्र संवाद को सुचारु बनाने और जटिल डाटा पठन को सरल करने के लिए करते हैं।

## कैसे करें:

```TypeScript
// वाक्यांश संयोजन के नमूना

let str1 = "नमस्ते, ";
let str2 = "दुनिया!";
let str3 = str1.concat(str2);
console.log(str3);  // आउटपुट: नमस्ते, दुनिया!

// लिट्रल टेम्पलेट सहित वाक्यांश संयोजन

let name = "रवि";
let greeting = `नमस्ते, ${name}!`;
console.log(greeting);  // आउटपुट: नमस्ते, रवि!
```
## गहरी डाइव:

(1) ऐतिहासिक संदर्भ में, जबसे कंप्यूटर बनाये गए हैं, वाक्यांश संयोजन का उपयोग हो रहा है। इसका मुख्य उपयोग मानव मशीन इंटरफ़ेस में होता है।

(2) विकल्पत: आप '+=' ऑपरेटर का भी इस्तेमाल कर सकते हैं।

``` TypeScript
let str1 = "नमस्ते, ";
str1 += "दुनिया!";
console.log(str1);  // आउटपुट: नमस्ते, दुनिया!
```

(3) आंतरिक विवरण: TypeScript में, वाक्यांशों को जोडने (जैसे की `concat` मथोद या '+=' ऑपरेटर) पर कार्य करते समय, अंतर्निहित जावास्क्रिप्ट एंजन नई स्ट्रिंग स्थान बनाता है, यदि किसी स्ट्रिंग को परिवर्तित किया गया है या नए स्ट्रिंग ने पुराने स्ट्रिंग को बदल दिया है।

## देखें भी:

- [MDN Web Docs on String Concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [TypeScript Handbook on String Manipulation](https://www.typescriptlang.org/docs/handbook/utility-types.html)
- [JavaScript & TypeScript concat method](https://www.w3schools.com/jsref/jsref_concat_string.asp)
---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
खोजना और पाठ्य बदलना एक प्रकार का काम है जिसमें हम किसी मूल शब्द को और दूसरे शब्द से प्रतिस्थापित करते हैं। इसका मुख्य उपयोग बड़ी फ़ाइलों या प्रोजेक्ट को संशोधित करने के लिए होता है, जिससे कोड को समझना और लिखना आसान होता है। 

## कैसे करें:
TypeScript में, हम `replace` फ़ंक्शन का इस्तेमाल कर सकते हैं। नीचे दिए गए कोड के साथ:

```TypeScript
let str = "Hello TypeScript!";
let newStr = str.replace("TypeScript", "World");
console.log(newStr);
```
इस कोड का आउटपुट होगा: 

```TypeScript
Hello World!
```

## गहन अध्ययन:
1. ऐतिहासिक परिप्रेक्ष्य: खोज और प्रतिस्थापन की कार्यक्षमता का समर्थन बहुत पुराने समय से उपलब्ध है, जब कंप्यूटर प्रोग्रामिंग नई-नई शुरू हुई थी।

2. विकल्प: TypeScript के अलावा, आप JavaScript, Java, Python, Ruby आदि जैसी अन्य भाषाओं में भी खोज और प्रतिस्थापन कर सकते हैं।

3. कार्यान्वयन विवरण: `replace` फ़ंक्शन का उपयोग स्ट्रिंग में पाठ बदलने के लिए किया जाता है। यदि मैच नहीं होता है, तो `replace` फ़ंक्शन शांत रूप से String के अस्पर्शित रूप को वापस करता है। 

## इसे भी देखें:
1. [TypeScript Handbook: String](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
2. [MDN Web Docs: String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
3. [JavaScript Search and Replace](https://www.w3schools.com/jsref/jsref_replace.asp)
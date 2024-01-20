---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

सर्च और रिप्लेस टेक्स्ट, अन्य किसी टेक्स्ट क्वेरी को खोजने और उसे नए टेक्स्ट से बदलने की क्रिया है। प्रोग्रामर्स इसे डाटा ऐनालिटिक्स, डिबगिंग और टेक्स्ट प्रोसेसिंग के लिए करते हैं। 

## कैसे करें: 

Javascript में, हम `String.prototype.replace()` और `RegExp` का उपयोग करके खोज और बदलाव कर सकते हैं। 

```Javascript 
let pangram = "The quick brown fox jumps over the lazy dog"
pangram = pangram.replace(/fox/g, "cat");
console.log(pangram)
```

आउटपुट:
```Javascript 
"The quick brown cat jumps over the lazy dog"
```

## गहरा गोताखोरी: 

खोजने और बदलने के विचारधारा का इतिहास कंप्यूटर विज्ञान के शुरुआती दिनों से है। इसपर अल्टरनेटिवों में खोजने के लिए `.indexOf()` और बदलने के लिए `.splice()` स्वरूपक मिलेंगे, लेकिन उन्हें `RegExp` की तरह शक्तिशाली नहीं माना जाता। अधिक जानकारी के लिए, आप [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace) पर विस्तार से पढ़ सकते हैं। 

## और भी देखें:

- [JavaScript String replace() Method](https://www.w3schools.com/jsref/jsref_replace.asp)
- [JavaScript RegExp Object](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [JavaScript String indexOf() Method](https://www.w3schools.com/jsref/jsref_indexof.asp)
- [JavaScript Array splice() Method](https://www.w3schools.com/jsref/jsref_splice.asp)
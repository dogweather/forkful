---
title:                "रेगुलर एक्सप्रेशन्स का उपयोग करना"
aliases:
- hi/javascript/using-regular-expressions.md
date:                  2024-02-03T19:19:27.796234-07:00
model:                 gpt-4-0125-preview
simple_title:         "रेगुलर एक्सप्रेशन्स का उपयोग करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

JavaScript में रेगुलर एक्सप्रेशन्स (regex) वे पैटर्न होते हैं जिनका इस्तेमाल स्ट्रिंग्स में वर्ण संयोजनों के मेल को खोजने के लिए किया जाता है। प्रोग्रामर्स इनका इस्तेमाल टेक्स्ट को खोजने, निकालने और मैनिपुलेट करने के लिए करते हैं, जिससे कम बूझत कोड के साथ शक्तिशाली स्ट्रिंग प्रोसेसिंग ऑपरेशन्स की अनुमति मिलती है।

## कैसे करें:

### बुनियादी मिलान

शुरू करने के लिए, आप एक सिंपल रेगुलर एक्सप्रेशन पैटर्न बना सकते हैं और इसे एक स्ट्रिंग में मेल खोजने के लिए इस्तेमाल कर सकते हैं। यहाँ, हम "code" शब्द खोजेंगे:

```javascript
const str = "मुझे JavaScript में कोड लिखना बहुत पसंद है।";
const pattern = /code/;
const result = pattern.test(str);
console.log(result); // true
```

### `String.prototype.match()` का इस्तेमाल करना

मेलों का एक ऐरे पुनः प्राप्त करने के लिए:

```javascript
const matches = str.match(/code/);
console.log(matches[0]); // "code"
console.log(matches.index); // 10
```

### ग्लोबल खोज

सभी मेलों को खोजने के लिए `g` फ्लैग का उपयोग करें:

```javascript
const globalMatches = str.match(/o/g);
console.log(globalMatches); // ["o", "o", "o"]
```

### केस-इनसेंसिटिव मैचिंग

`i` फ्लैग केस को अनदेखा करता है:

```javascript
const caseInsensitiveMatch = "JavaScript मजेदार है".match(/javascript/i);
console.log(caseInsensitiveMatch[0]); // "JavaScript"
```

### टेक्स्ट को प्रतिस्थापित करना

टेक्स्ट के कुछ हिस्सों को प्रतिस्थापित करने के लिए `String.prototype.replace()` का इस्तेमाल करें:

```javascript
const newStr = "JavaScript मजेदार है".replace(/मजेदार/, "अद्भुत");
console.log(newStr); // "JavaScript अद्भुत है"
```

### समूहों का इस्तेमाल

समूह पैटर्न के कुछ हिस्सों को कैप्चर कर सकते हैं:

```javascript
const groupedPattern = /(\w+) है (\w+)/;
const replaceWithGroups = "JavaScript मजेदार है".replace(groupedPattern, "$2 है $1");
console.log(replaceWithGroups); // "मजेदार है JavaScript"
```

### थर्ड-पार्टी लाइब्रेरीज

भले ही JavaScript की बिल्ट-इन रेगुलर एक्सप्रेशन क्षमताएं शक्तिशाली हैं, कुछ कार्यों को `XRegExp` जैसी लाइब्रेरी के साथ सरल किया जा सकता है। इसमें अतिरिक्त सिंटेक्स और फ्लैग्स की पेशकश की जाती है, जिससे जटिल पैटर्न और अधिक पढ़ने योग्य होते हैं:

```javascript
// XRegExp लाइब्रेरी का उदाहरण
const XRegExp = require('xregexp');
const str = "बिल्लियाँ अद्भुत होती हैं।";
const unicodeWordMatch = XRegExp.match(str, XRegExp('\\p{L}+'), 'all');
console.log(unicodeWordMatch); // ["बिल्लियाँ", "अद्भुत", "होती"]
```

यह स्निपेट `XRegExp` का उपयोग करके एक स्ट्रिंग में सभी Unicode शब्दों को मैच करने का तरीका दिखाता है, जिससे पता चलता है कि लाइब्रेरी JavaScript की बिल्ट-इन क्षमताओं के परे विस्तृत वर्ण सेट को संभालने में सक्षम है।

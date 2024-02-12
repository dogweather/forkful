---
title:                "सहयोगी अरेज़ का उपयोग करना"
date:                  2024-01-30T19:14:14.086391-07:00
model:                 gpt-4-0125-preview
simple_title:         "सहयोगी अरेज़ का उपयोग करना"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

एसोसिएटिव ऐरे या TypeScript में ऑब्जेक्ट्स, आपको मान जोड़े तक पहुँचने के लिए स्ट्रिंग्स (या कुंजियों) का उपयोग करने की अनुमति देते हैं। प्रोग्रामर्स इनका उपयोग पारंपरिक ऐरेज़ की तुलना में अधिक डायनामिक डाटा पहुँच पैटर्न के लिए करते हैं, जो बिना संख्यात्मक सूचकांकों से जुड़े हुए डाटा को संरचित और पहुँच प्रदान करने का एक लचीला तरीका देता है।

## कैसे करें:

TypeScript में एसोसिएटिव ऐरे बनाना और उपयोग करना सरल है। यहाँ एक मूल रन-थ्रू है:

```TypeScript
// एसोसिएटिव ऐरे घोषित करना
let user: { [key: string]: string } = {};

// डेटा जोड़ना
user["name"] = "Jane Doe";
user["email"] = "jane@example.com";

console.log(user);
```

आउटपुट:

```TypeScript
{ name: 'Jane Doe', email: 'jane@example.com' }
```

कुंजी-मान जोड़ों के पार इटरेटिंग भी आसान है:

```TypeScript
for (let key in user) {
    console.log(key + ": " + user[key]);
}
```

आउटपुट:

```TypeScript
name: Jane Doe
email: jane@example.com
```

और यदि आप विविध डाटा प्रकारों के साथ काम कर रहे हैं, तो TypeScript की टाइप सिस्टम काम आती है:

```TypeScript
let mixedTypes: { [key: string]: string | number } = {};
mixedTypes["name"] = "John Doe";
mixedTypes["age"] = 30;

console.log(mixedTypes);
```

आउटपुट:

```TypeScript
{ name: 'John Doe', age: 30 }
```

## गहराई से जानकारी

TypeScript में, हम जिन्हें एसोसिएटिव ऐरे कहते हैं, वे मूल रूप से ऑब्जेक्ट्स हैं। पारंपरिक रूप से, पीएचपी जैसी भाषाओं में, एसोसिएटिव ऐरे एक मौलिक प्रकार हैं, लेकिन जावास्क्रिप्ट (और विस्तार से, TypeScript) इस उद्देश्य के लिए ऑब्जेक्ट्स का उपयोग करता है। यह दृष्टिकोण एक ताकत और एक सीमा दोनों है। ऑब्जेक्ट्स मानों से स्ट्रिंग्स को जोड़ने के लिए एक बेहद डायनामिक संरचना प्रदान करते हैं, लेकिन इनका उपयोग पारंपरिक अर्थ में 'ऐरेज़' के रूप में नहीं किया जाना चाहिए। उदाहरण के लिए, आप इन ऑब्जेक्ट्स पर सीधे ऐरे मेथड्स जैसे कि `push` या `pop` का उपयोग नहीं कर सकते।

जहाँ आपको कुंजी-मान जोड़ों के साथ आर्डर्ड संग्रहों की आवश्यकता होती है और ऐरे-जैसे ऑपरेशंस चाहिए, TypeScript (और आधुनिक जावास्क्रिप्ट) आपको `Map` ऑब्जेक्ट प्रदान करता है:

```TypeScript
let userMap = new Map<string, string>();
userMap.set("name", "Jane Doe");
userMap.set("email", "jane@example.com");

userMap.forEach((value, key) => {
    console.log(key + ": " + value);
});
```

जबकि TypeScript की टाइप सिस्टम और ES6 फीचर्स जैसे `Map` शक्तिशाली विकल्प प्रदान करते हैं, ऑब्जेक्ट लिटरल्स के रूप में एसोसिएटिव ऐरे का उपयोग कैसे करें, यह समझना स्थितियों के लिए उपयोगी है जहाँ ऑब्जेक्ट लिटरल्स अधिक कुशल होते हैं या जब JSON डाटा संरचनाओं के साथ काम कर रहे हों। यह सब सही उपकरण के चुनाव के बारे में है।

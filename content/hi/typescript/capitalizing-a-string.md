---
title:                "TypeScript: स्ट्रिंग कैपिटलाइजिंग"
simple_title:         "स्ट्रिंग कैपिटलाइजिंग"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

कैसे आप किसी भी मूल्य को अग्रणी अक्षरों में बदल सकते हैं? एक शब्द की पहचान पर विचार करें, जिसमें आपको हर समय पर्याप्त रूप से पता चल जाता है कि वह शब्द क्या है। उदाहरण के लिए, दिनांक, वेबसाइट नाम या उपयोगकर्ता नाम। इस प्रकार, जब हम किसी शब्द को अपने कोड में उपयोग करते हैं, हमारे लिए उसे अग्रणी अक्षरों में प्रदर्शित करना आसान होता है। इसलिए, यह आवश्यक हो सकता है कि किसी भी मूल्य को अग्रणी अक्षरों में कैसे बदला जाए।

## कैसे करें

```TypeScript
function capitalize(str: string) {
  return str.toUpperCase();
}

console.log(capitalize("hello")); // output: HELLO
console.log(capitalize("how are you")); // output: HOW ARE YOU
```

```TypeScript
const name = "john";
console.log(`Welcome, ${capitalize(name)}.`); // output: Welcome, JOHN.
```

## गहराई में जाएँ

एक और सरल तरीके से आप अपने वर्तमान कोड को पोलिश कर सकते हैं। आप एक प्रोमीज़ को इस्तेमाल करके अग्रणी अक्षरों में बदलने का एक प्रयास कर सकते हैं।

```TypeScript
function capitalizePromise(str: string): Promise<string> {
  return new Promise((resolve, reject) => {
    if (typeof str !== "string") {
      reject("Argument must be a string.");
    } else {
      resolve(str.toUpperCase());
    }
  });
}

capitalizePromise("bye")
  .then(result => console.log(result)) // output: BYE
  .catch(error => console.log(error));
```

```TypeScript
const sentence = "i am learning typescript";
capitalizePromise(sentence)
  .then(result => console.log(`Now I can say: ${result}.`)) // output: Now I can say: I AM LEARNING TYPESCRIPT.
  .catch(error => console.log(error));
```

## देखें भी

[MDN अक्षरों को अपडेट करना](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
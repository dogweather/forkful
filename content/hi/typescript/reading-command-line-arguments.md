---
title:                "TypeScript: सूचना लाइन तार्किक तरिके का पठन"
simple_title:         "सूचना लाइन तार्किक तरिके का पठन"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्यों

कमांड लाइन आर्गुमेंट पढ़ने का मतलब यह है कि आप अपने प्रोग्राम में डेटा दर्ज कर सकते हैं जो आपको दूसरों से प्राप्त करने से बचाता है।

## कैसे करें

कमांड लाइन आर्गुमेंट पढ़ने के लिए आपको सबसे पहले अपने प्रोग्राम में `process` ऑब्जेक्ट इम्पोर्ट करना होगा। फिर आप फाइल के साथ `process.argv` वेरिएबल जोड़ सकते हैं जो आपको सभी कमांड लाइन आर्गुमेंट्स की सूची देगा। उदाहरण के लिए, यदि आपका फाइल `index.ts` है और आपको 3 आर्गुमेंट पढ़ने हैं तो आप निम्नलिखित कोड का उपयोग कर सकते हैं:

```TypeScript
import process from "process";

let argument1 = process.argv[2];
let argument2 = process.argv[3];
let argument3 = process.argv[4];

console.log(argument1, argument2, argument3);
```

इसका आउटपुट निम्न होगा:

```bash
$ ts-node index.ts hello world !
hello world !
```

## गहराई की तरफ

कई बार, हमारे पास अधिकतम संख्या या निर्दिष्ट प्रकार के कमांड लाइन आर्गुमेंट्स हो सकते हैं। इस स्थिति में, हम `if/else` चेकिंग या `switch` लोजिक का उपयोग कर सकते हैं जो आर्गुमेंट्स को विभिन्न वेरिएबल्स में स्थानांतरित करेंगे। इससे हमारे पास आसानी से पहुंच और व्यवस्था रहती है, जो हमारे प्रोग्राम को लंबे समय तक दौरान साफ और स्पष्ट रखेगा।

## देखें भी

- [TypeScript ऑफिशियल डॉक्यूमेंटेशन](https://www.typescriptlang.org/docs/)
- [कमांड लाइन आर्गुमेंट्स को पढ़ने का वीडियो ट्यूटोरियल](https://www.youtube.com/watch?v=qCdCeXAMf
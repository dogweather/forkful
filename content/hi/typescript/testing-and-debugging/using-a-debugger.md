---
title:                "डीबगर का उपयोग करना"
date:                  2024-01-26T04:12:59.536583-07:00
model:                 gpt-4-0125-preview
simple_title:         "डीबगर का उपयोग करना"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/using-a-debugger.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
डिबगर एक उपकरण है जो आपको अपने कोड के आंतरिक कामकाज का परीक्षण और परिवर्तन करने की अनुमति देता है, जबकि यह चल रहा होता है। प्रोग्रामर इसका उपयोग अपने कोड के माध्यम से कदम-दर-कदम जांच करके, वेरिएबल्स की जांच करके, और अपने प्रोग्राम की प्रवाह को समझकर बग्स को दूर करने के लिए करते हैं।

## कैसे करें:

TypeScript में एक डिबगर के साथ शुरुआत करने के लिये, आपको एक समर्थित IDE (जैसे की Visual Studio Code) और एक `launch.json` कॉन्फ़िगरेशन की ज़रूरत है। यहां एक Node.js एप्लिकेशन के लिए एक त्वरित उदाहरण है:

```TypeScript
// app.ts
function greet(name: string) {
    console.log(`Hello, ${name}!`);
}

const userName = 'Ada';
greet(userName);
```

इसे डिबग करने के लिए, `.vscode` फ़ोल्डर के नीचे एक `launch.json` फ़ाइल बनाएं:

```JSON
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "node",
            "request": "launch",
            "name": "Launch Program",
            "skipFiles": ["<node_internals>/**"],
            "program": "${workspaceFolder}/app.ts",
            "preLaunchTask": "tsc: build - tsconfig.json",
            "outFiles": ["${workspaceFolder}/build/**/*.js"]
        }
    ]
}
```

फिर, अपने `greet` फ़ंक्शन में एक ब्रेकप्वाइंट सेट करें, अपने IDE में लाइन नंबर के बाईं तरफ पर क्लिक करके। F5 दबाकर डिबगिंग शुरू करें, और अपने ऐप को ब्रेकप्वाइंट पर रुकते हुए देखें। अब आप वेरिएबल्स पर होवर कर सकते हैं, एक्सप्रेशंस पर नज़र रख सकते हैं, और अपने कोड के माध्यम से आसानी से कदम-दर-कदम बढ़ सकते हैं।

## गहराई में जानकारी

बहुत पहले, जब इंटीग्रेटेड डेवलपमेंट एन्वायरनमेंट्स (IDEs) चिकने नहीं हुए थे, तब अक्सर प्रिंट स्टेटमेंट्स (जिन्हें `console.log` डिबगिंग कहा जाता था) के साथ डिबगिंग की जाती थी। यह कार्य करता था, किसी तरह से, लेकिन यह आंखों पर पट्टी बांधकर घास के ढेर में सुई खोजने जैसा था। 

आधुनिक डिबगर्स ट्रबल्शूटिंग के लिए स्विस आर्मी नाइफ की तरह हैं। TypeScript और Node.js के विकास के साथ, बिल्ट-इन Node.js इंस्पेक्टर से लेकर क्लाइंट-साइड डिबगिंग के लिए ब्राउज़र डेव टूल्स तक, विभिन्न डिबगर्स उपलब्ध हैं। 

Node.js इंस्पेक्टर आपके चल रहे एप्लिकेशन से अटैच होकर काम करता है; यह Chrome DevTools Protocol के ज़रिए संवाद करता है, जिससे आपका Chrome ब्राउज़र एक शक्तिशाली डिबगिंग कंसोल में बदल जाता है। यह एकीकरण पारंपरिक कमांड-लाइन डिबगिंग प्रथाओं की तुलना में एक दृश्यात्मक रूप से इंटरैक्टिव और विस्तृत डिबगिंग सत्र की अनुमति देता है।

## और देखें

कुछ अतिरिक्त पढ़ाई और कुछ प्रो-टिप्स के लिए, देखें:

- [Visual Studio Code में TypeScript डिबगिंग](https://code.visualstudio.com/docs/typescript/typescript-debugging)
- [Node.js डिबगिंग गाइड](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [Chrome DevTools प्रलेखन](https://developers.google.com/web/tools/chrome-devtools)

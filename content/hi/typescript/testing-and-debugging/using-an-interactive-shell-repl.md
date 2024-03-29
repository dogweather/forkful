---
date: 2024-01-26 04:20:02.508804-07:00
description: "\u0930\u0940\u0921-\u090F\u0935\u0948\u0932-\u092A\u094D\u0930\u093F\
  \u0902\u091F-\u0932\u0942\u092A (REPL) \u090F\u0915 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u093F\u0902\u0917 \u0935\u093E\u0924\u093E\u0935\u0930\u0923\
  \ \u0939\u0948 \u091C\u094B \u090F\u0915\u0932 \u0909\u092A\u092F\u094B\u0917\u0915\
  \u0930\u094D\u0924\u093E \u0907\u0928\u092A\u0941\u091F\u094D\u0938 \u0915\u094B\
  \ \u0932\u0947\u0924\u093E \u0939\u0948, \u0909\u0928\u094D\u0939\u0947\u0902 \u0928\
  \u093F\u0937\u094D\u092A\u093E\u0926\u093F\u0924 \u0915\u0930\u0924\u093E \u0939\
  \u0948, \u0914\u0930 \u092A\u0930\u093F\u0923\u093E\u092E \u0915\u094B \u0909\u092A\
  \u092F\u094B\u0917\u0915\u0930\u094D\u0924\u093E \u0915\u0947 \u092A\u093E\u0938\
  \ \u0935\u093E\u092A\u093F\u0938\u2026"
lastmod: '2024-03-13T22:44:51.891934-06:00'
model: gpt-4-0125-preview
summary: "\u0930\u0940\u0921-\u090F\u0935\u0948\u0932-\u092A\u094D\u0930\u093F\u0902\
  \u091F-\u0932\u0942\u092A (REPL) \u090F\u0915 \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u093F\u0902\u0917 \u0935\u093E\u0924\u093E\u0935\u0930\u0923\
  \ \u0939\u0948 \u091C\u094B \u090F\u0915\u0932 \u0909\u092A\u092F\u094B\u0917\u0915\
  \u0930\u094D\u0924\u093E \u0907\u0928\u092A\u0941\u091F\u094D\u0938 \u0915\u094B\
  \ \u0932\u0947\u0924\u093E \u0939\u0948, \u0909\u0928\u094D\u0939\u0947\u0902 \u0928\
  \u093F\u0937\u094D\u092A\u093E\u0926\u093F\u0924 \u0915\u0930\u0924\u093E \u0939\
  \u0948, \u0914\u0930 \u092A\u0930\u093F\u0923\u093E\u092E \u0915\u094B \u0909\u092A\
  \u092F\u094B\u0917\u0915\u0930\u094D\u0924\u093E \u0915\u0947 \u092A\u093E\u0938\
  \ \u0935\u093E\u092A\u093F\u0938\u2026"
title: "\u0907\u0902\u091F\u0930\u0948\u0915\u094D\u091F\u093F\u0935 \u0936\u0947\u0932\
  \ (REPL) \u0915\u093E \u0909\u092A\u092F\u094B\u0917"
---

{{< edit_this_page >}}

## क्या और क्यों?
रीड-एवैल-प्रिंट-लूप (REPL) एक प्रोग्रामिंग वातावरण है जो एकल उपयोगकर्ता इनपुट्स को लेता है, उन्हें निष्पादित करता है, और परिणाम को उपयोगकर्ता के पास वापिस भेजता है। प्रोग्रामर्स कोड स्निपेट्स के साथ जल्दी प्रयोग करने, डिबगिंग, और बिना पूर्ण एप्लिकेशन बनाने की अधिकता के बिना नई भाषा सुविधाओं को सीखने के लिए REPL का उपयोग करते हैं।

## कैसे:
TypeScript अपने स्वयं के REPL के साथ नहीं आता है। चलो `ts-node` का उपयोग करें, एक TypeScript निष्पादन वातावरण Node.js के लिए जिसमें REPL शामिल है।

पहले, इसे विश्वव्यापी स्थापित करें:
```bash
npm install -g ts-node
```

कमांड लाइन में `ts-node` टाइप करके REPL शुरू करें:
```bash
ts-node
```

आज़माने के लिए एक त्वरित स्निपेट यहाँ है:
```TypeScript
> let message: string = 'Hello, REPL!';
> console.log(message);
Hello, REPL!
> 
```
सत्र समाप्त करने के लिए, `Ctrl+D` दबाएं।

## गहन जानकारी
ऐतिहासिक रूप से, REPLs Lisp जैसी भाषाओं में प्रमुख थे, जो गतिशील कोड मूल्यांकन की अनुमति देते थे। अवधारणा तब से फैल गई है, कई भाषाओं में इंटरैक्टिव कोडिंग के लिए एक स्थाई बन गई है।

TypeScript के लिए, `ts-node` आपका एकमात्र विकल्प नहीं है। वैकल्पिक में वेब ब्राउज़र में TypeScript प्लेग्राउंड का उपयोग करना या उपयुक्त प्लगइन्स के साथ TypeScript का समर्थन करने वाले अन्य Node.js-आधारित REPLs का लाभ उठाना शामिल है।

क्रियान्वयन के मामले में, `ts-node` TypeScript कॉम्पाइलर API का उपयोग करके कोड को उड़ान में ट्रांसपाइल करता है इससे पहले कि यह Node.js द्वारा निष्पादित किया जाता है। यह तत्काल प्रतिक्रिया देता है और विशेष रूप से TypeScript की नवीनतम सुविधाओं का प्रयास करने के लिए उपयोगी है बिना सेटअप की परेशानियों के।

एक बात याद रखना - जबकि REPL त्वरित परीक्षणों के लिए महान है, यह पारंपरिक, परीक्षण योग्य, और रखरखाव योग्य कोड लिखने को प्रतिस्थापित नहीं करता है। यह सीखने और अन्वेषण के लिए एक उपकरण है, उचित विकास प्रथाओं के लिए एक विकल्प नहीं है।

## देखें भी
- [TypeScript आधिकारिक वेबसाइट](https://www.typescriptlang.org/)
- [GitHub पर ts-node](https://github.com/TypeStrong/ts-node)
- [Node.js REPL दस्तावेज़ीकरण](https://nodejs.org/api/repl.html)
- [TypeScript प्लेग्राउंड](https://www.typescriptlang.org/play)

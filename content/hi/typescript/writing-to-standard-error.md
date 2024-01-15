---
title:                "स्टैंडर्ड त्रुटि पर लिखना"
html_title:           "TypeScript: स्टैंडर्ड त्रुटि पर लिखना"
simple_title:         "स्टैंडर्ड त्रुटि पर लिखना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्यों

स्टैंडर्ड एरर में लिखने के तरीके को समझने से पहले, हमें इसे क्यों किया जाता है यह समझना जरूरी है। यह एक त्रुटि है जो कोडिंग के दौरान हो सकती है और उसे ठीक करना आसान होता है अगर हम इसे स्टैंडर्ड एरर में लिखते हैं।

## कैसे करें

एक सरल उदाहरण के माध्यम से यह समझना सरल होगा। सामान्य रूप से, हम स्टैंडर्ड एरर को `console.error()` फंक्शन का उपयोग करके लिखते हैं। नीचे दिए गए कोड ब्लॉक में उदाहरण दिखाया गया है:

```TypeScript
console.error("यह एक स्टैंडर्ड एरर है।");
```

इसका आउटपुट निम्न होगा:

```
यह एक स्टैंडर्ड एरर है।
```

इस तरह से, हम कोडिंग के दौरान आने वाली ट्रुटियों को स्टैंडर्ड एरर में लिख सकते हैं।

## गहराई में जाओ

स्टैंडर्ड एरर में लिखना काफी उपयोगी हो सकता है। रिएक्शन त्रुटि की जांच करने, लॉगिंग के लिए या उपयोगकर्ता को अपने कोड में किसी ट्रुटि के बारे में अवगत कराने के लिए इसे उपयोग किया जा सकता है। स्टैंडर्ड एरर में लिखने की अन्य विशेषताओं को आप [यहां](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-9.html#error-logging) से पढ़ सकते हैं।

## देखें भी

- [TypeScript डॉक्स](https://www.typescriptlang.org/docs/)
- [TypeScript फॉरमेटिंग और स्टाइल गाइड](https://github.com/Microsoft/TypeScript/wiki/Coding-guidelines)
- [TypeScript के Github पेज](https://github.com/Microsoft/TypeScript)
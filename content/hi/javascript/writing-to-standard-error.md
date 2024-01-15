---
title:                "स्टैंडर्ड एरर में लिखना"
html_title:           "Javascript: स्टैंडर्ड एरर में लिखना"
simple_title:         "स्टैंडर्ड एरर में लिखना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्यों

कुछ समय जब हम प्रोग्राम लिखते हैं, तो हमे अपने कोड में कोई गलती या अनुचितता मिलती है। ऐसे स्थिति में, हम स्टैंडर्ड एरर को उठाना चाहते हैं जो हमे अपनी गलतियों का पता लगाने और सुधार करने का मौका देता है। इसलिए, स्टैंडर्ड एरर लिखना एक महत्वपूर्ण कौशल है जो हर प्रोग्रामर को जानना चाहिए। 

## कैसे करे

```javascript 
console.error("यह मैसेज स्टैंडर्ड एरर में लिखा है।");
```

इस प्रकार से, हम `console.error()` फंक्शन का उपयोग करके स्टैंडर्ड एरर में संदेश लिख सकते हैं। वैसे तो `console.log()` भी काम करता है, लेकिन स्टैंडर्ड एरर में लिखना कोड में गलती को पकड़ने के लिए अधिक उपयोगी होगा। 

## गहराई में

स्टैंडर्ड एरर में लिखना एक महत्वपूर्ण कौशल है जो हर प्रोग्रामर को जानता होना चाहिए। `console.error()` के अलावा, `process.stderr.write()` भी स्टैंडर्ड एरर में लिखने के लिए उपयोगी हो सकता है। इसके अलावा, हम अपने कोड में `try-catch` ब्लॉक का भी उपयोग करके अपनी गलतियों को हैंडल कर सकते हैं। 

## देखें भी

- [MDN Documentation on console.error()](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
- [Stack Overflow thread on using console.error()](https://stackoverflow.com/questions/1593090/list-of-what-console-errors-can-i-capture) 
- [Node.js Documentation on process.stderr.write()](https://nodejs.org/api/process.html#process_process_stderr)
- [GeeksforGeeks article on Error Handling in JavaScript](https://www.geeksforgeeks.org/error-handling-in-javascript/)
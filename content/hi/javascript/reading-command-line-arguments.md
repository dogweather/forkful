---
title:                "कम्प्यूटर प्रोग्रामिंग पर आधारित: कमांड लाइन तर्क पठन"
html_title:           "Javascript: कम्प्यूटर प्रोग्रामिंग पर आधारित: कमांड लाइन तर्क पठन"
simple_title:         "कम्प्यूटर प्रोग्रामिंग पर आधारित: कमांड लाइन तर्क पठन"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## यह क्या है और क्यों?
कमांड लाइन आर्ग्यूमेंट पढ़ना क्या है, इसके बारे में दो-तीन वाक्यों में समझाने की कोशिश करते हैं। इसका मतलब है कि हम अपने कोड को किसी कमांड लाइन से पास किए गए आर्ग्यूमेंट परिवर्तित कर सकते हैं, जो हमें अपनी अनुकूलता के अनुसार कार्य कराने में मदद करता है। इसका प्रयोग करने से हम अपने कोड को दोहराने से बच सकते हैं और अपनी कार्य प्रणाली को सुचारू बना सकते हैं।

## कैसे करें:
```Javascript
// कॉमन्ट: अंतराल से अलग किया गया आर्ग्यूमेंट रीड करने का उदाहरण
const args = process.argv.slice(2);
console.log(args);

// इनपुट: node index.js hello world
// आउटपुट: [ 'hello', 'world' ]
```

## गूर्घाशी:
कमांड लाइन आर्ग्यूमेंट पढ़ने के इतिहास की बात करें, तो साल 1969 में एक निजी क्रॉस-प्लेटफ़ॉर्म सिस्टम वाले कंप्यूटर विज्ञानी डगलस माकरोयन ने पहला कमांड लाइन इंटरेकेटिव शेल का निर्माण किया। यह लोकप्रिय युक्तियों में से एक है जो प्रोग्रामिंग भाषाओं का आविष्कार करते हुए कई नए तरीकों से कोड लिखने में हमें मदद करती है। दूसरे विकल्प के रूप में, हम यह निष्कर्ष निकाल सकते हैं कि कमांड लाइन आर्ग्यूमेंट पढ़ना एक बहुत ही उपयोगी डेवलपर फीचर है। अतः, हम अपने कोड में इसका प्रयोग करना कभी भी न भूलें।

## और भी देखें:
- [Node.js documentation on Command-line Arguments](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Medium article on Understanding Command-Line Arguments in Node.js](https://medium.com/@sumn2u/understanding-command-line-arguments-in-node-js-a530d1f1c233)
- [GeeksforGeeks article on Command-Line Arguments in JavaScript](https://www.geeksforgeeks.org/command-line-arguments-in-javascript/)
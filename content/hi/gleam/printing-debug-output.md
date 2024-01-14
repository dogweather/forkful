---
title:                "Gleam: डीबग आउटपुट को प्रिंट करना"
simple_title:         "डीबग आउटपुट को प्रिंट करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्यों:

डीबग आउटपुट छापने का मानदंडानुसार क्यों करना चाहिए- किसी को डीबग कोड को समझने में वेस्ट काम से बचाने के लिए या कोड में भूलों को खोजने के लिए। 

## कैसे करे:

आपके ग्रीम कोड में इस्तेमाल होने वाली छाप डीबग का उदाहरण और उससे मिलने वाला आउटपुट निम्नलिखित “Gleam …” लिखित कोड ब्लाक में दर्शाया गया है। 

```Gleam code
fn main() {
    let name = "Gleam";
    gleam_debug::debug("Hello, " ++ name ++ "!");
}
```

आउटपुट:

```
Hello, Gleam!
```

## गहरी जानकारी:

डीबग आउटपुट छापने के लिए विभिन्न तकनीकों में क्या अंतर हो सकता है, उनमें कौन से प्रोटोकॉल और पेटर्न उपयोग किया जा सकता है, और कोड के अनुमानित या प्रत्याशित डीबग आउटपुट से कौन से भूलों का पता लगाया जा सकता है। 

## देखिए भी:

देखिए भी (See Also):

- [Gleam डॉक्यूमेंटेशन](https://gleam.run/getting-started/)
- [प्रोग्रामिंग हिंदी में जानकारी](https://programming-learn.blogspot.com/) 
- [हिंदी में कंप्यूटर साइंस ट्यूटोरियल](https://hindi.hackr.io/tutorials/computer-science)
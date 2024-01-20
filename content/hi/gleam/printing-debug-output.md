---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

Debug output का प्रिंट करना मतलब कठिनाईयों (bugs) को देखने के लिए कोड में अस्थाई मूल्यों का उपयोग करना। यह प्रोग्रामर्स को कोड कैसे काम कर रहा है, इसकी समझ देता है।

## कैसे करें:

Gleam में आप `gleam/io` के `debug` function का उपयोग करके debug output को print कर सकते हैं।

```gleam
import gleam/io

fn main() {
  let x = "Debug Data"
  io.debug(x)
}
```

इसे चलाने पर, आपको terminal में "Debug Data" मिलेगा।

```output
Debug Data
```

## Deep Dive:

**ऐतिहासिक सन्दर्भ:** Debugging तकनीक प्राचीन प्रोग्रामिंग के इतने ही पुराने हैं जितनी कंप्यूटर। यह हमेशा से प्रोग्रामर्स के उपकरण किट का हिस्सा रहा है।

**विकल्प:** कुछ विकल्प `print` statements हो सकते हैं, लेकिन `io.debug` function का उपयोग करना अधिक structured और मानकीकृत रूप से डेटा को लॉग करने का एक बेहतर दर्शाता है।

**कार्यान्वयन विवरण:** Gleam में `io.debug` function, डिबग्गिंग जानकारी को आउटपुट करता है, और यह Erlang/OTP के निविदान पर आधारित है।

## See Also:

- [Gleam Documentation - io.debug](https://hexdocs.pm/gleam_erlang_stdlib/gleam/io/)
- [Debugging Techniques](https://en.wikipedia.org/wiki/Debugging)
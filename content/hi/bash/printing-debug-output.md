---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

प्रिंटिंग डीबगग आउटपुट से यह समझा जाता है कि किस तरह की प्रक्रिया कोड में कार्यरत है और इसका उपयोग करके, हमने किसी समस्या की पहचान करने में मदद की है। 

## कैसे करें:

पृष्ठभूमि कार्यों में यह सामान्य तरीका होता है।

```Bash
#!/bin/bash
echo "Debugging start" >&2
command1
command2
echo "Debugging end" >&2
```
इसमें `echo` कमांड्स Debugging की प्रक्रिया को मार्क करने के लिए उपयोग होती हैं। `>&2` यह सुनिश्चित करता है कि यह मैंन स्ट्रीम पर साझा नहीं किया जा रहा है।

## गहन अध्ययन:

Bash शेल स्क्रिप्टिंग के प्रारंभिक दिनों से ही डीबग आउटपुट का उपयोग किया जा रहा है। इसका मुख्य विकल्प `set -x` और `set +x` होता हैं, जिसे टर्न ऑन और टर्न ऑफ करने के लिए उपयोग किया जाता है। इसके अलावा, डीबगिंग के अन्य उपकरण भी हैं, जैसे `DShellCheck`, `BATS` और `shfmt`। किंतु, इन्हें उपयोग करने का तरीका और संम्भवतः इनपुट को प्राप्त करने का तरीका अधिक जटिलतमापूर्ण हो सकता है।

## अन्य स्रोत:

1. [Bash Debugging Guide](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_02_03.html) - यह गाइड उन लोगों के लिए है जो Bash Debugging की गहरी समझ प्राप्त करना चाहते हैं।
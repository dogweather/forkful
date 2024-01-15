---
title:                "पदच्छेद से निस्कारण"
html_title:           "Gleam: पदच्छेद से निस्कारण"
simple_title:         "पदच्छेद से निस्कारण"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्यों
चिंटियाँ आपके धीमे गेम में सबसे टेज क्राश हैं! ये स्ट्रिंग सब्स्ट्रिंग्स को निकालने का सबसे अनुकूल तरीका है, जो आपको गेम डेवलपमेंट के लिए शानदार परिणाम देगा।

## कैसे करें
```Gleam
let string = "हमारे घर में सबसे पहले दूरियां लगाई जाती हैं"
let substring = String.substring(string, 11, 15)
io.print(substring) // घर में सबसे पहले 
io.print(string) // हमारे घर में सबसे पहले दूरियां लगाई जाती हैं

let word = "मैं एक छोटी सी चिंटी हूँ"
let substring = String.substring(word, 7)
io.print(substring) // चिंटी हूँ
```

## डीप डाइव
स्ट्रिंग सब्स्ट्रिंग का उपयोग आपको विभिन्न विभिन्न स्थितियों में किया जा सकता है, जैसे कि टेक्स्ट प्रोसेसिंग, पाठ विश्लेषण और डाटा मैनिपुलेशन। आपको स्ट्रिंग सब्स्ट्रिंग का सही इस्तेमाल करके उसकी अधिक स्पष्टता और उपयोगिता को समझने के लिए एक गहराई से जानना चाहिए।

## देखें भी
- Official Gleam documentation (https://gleam.run)
- Tutorial on string manipulation in Gleam (https://gleam.run/tutorials/strings/)
- Example code for extracting substrings (https://github.com/gleam-lang/gleam/blob/master/examples/substring.gleam)
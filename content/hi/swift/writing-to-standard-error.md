---
title:                "मानक त्रुटि में लिखना"
date:                  2024-01-19
html_title:           "Arduino: मानक त्रुटि में लिखना"
simple_title:         "मानक त्रुटि में लिखना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
‘Standard error’ (stderr) एक स्ट्रीम होती है, जहाँ प्रोग्राम्स अपनी गलतियाँ और डायग्नोस्टिक मैसेजेस लिखते हैं ताकि यूजर को पता चल सके कि क्या सही नहीं है। प्रोग्रामर इसका उपयोग ऑउटपुट (stdout) और एरर मैसेजेस को अलग करने के लिए करते हैं।

## How to: (कैसे करें:)
Swift में `FileHandle` का उपयोग करके stderr पर लिखें:

```Swift
import Foundation

// Standard Error में मैसेज लिखना
func writeError(_ message: String) {
    if let data = "\(message)\n".data(using: .utf8) {
        FileHandle.standardError.write(data)
    }
}

// उदाहरण
writeError("यह एक एरर मैसेज है")
```

सैंपल आउटपुट:
```
यह एक एरर मैसेज है
```

## Deep Dive (गहराई से जानकारी)
Standard error stream (stderr) UNIX/Linux सिस्टम्स की एक विशेषता है जिसे अन्य ऑपरेटिंग सिस्टम्स ने भी अपनाया है। ऑउटपुट (stdout) के साथ मिलकर, यह प्रोग्रामर को डेटा और लॉग मैसेजेस के बीच फर्क करने का मौका देता है। `print` स्टेटमेंट से stdout पर लिखना आसान होता है, लेकिन stderr लिखने के लिए `FileHandle` या निचले स्तर के POSIX APIs की जरूरत होती है।

## See Also (और जानकारी)
और अधिक जानकारी के लिए, स्विफ्ट के आधिकारिक डॉक्युमेंटेशन और निम्नलिखित संसाधनों को देखें:

- [Swift.org Documentation](https://docs.swift.org/swift-book/index.html)
- [Apple Developer Documentation](https://developer.apple.com/documentation)
- [Unix stderr Documentation](https://man7.org/linux/man-pages/man3/stderr.3.html)

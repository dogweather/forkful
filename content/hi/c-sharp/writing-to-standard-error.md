---
title:                "मानक त्रुटि में लिखना"
date:                  2024-01-19
simple_title:         "मानक त्रुटि में लिखना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Standard error (stderr) एक output stream है जहां आप अपने प्रोग्राम की गलतियाँ या डीबग संदेश दिखा सकते हैं। प्रोग्रामर इसका उपयोग करते हैं ताकि गलतियों की जानकारी को स्टैंडर्ड आउटपुट (stdout) से अलग रख सकें, जिससे लॉगिंग और डीबग करना आसान हो।

## How to: (कैसे करें:)
```C#
using System;

class Program
{
    static void Main()
    {
        Console.WriteLine("स्टैंडर्ड आउटपुट पर हूँ!"); // Regular message
        Console.Error.WriteLine("स्टैंडर्ड एरर पर हूँ!"); // Error message
    }
}
```
सैंपल आउटपुट:
```
स्टैंडर्ड आउटपुट पर हूँ!
स्टैंडर्ड एरर पर हूँ!
```

## Deep Dive (गहराई में जानकारी)
Standard error, स्टैंडर्ड आउटपुट और स्टैंडर्ड इनपुट के साथ Unix कॉन्सेप्ट से आया है। ये तीनों स्ट्रीम्स होते हैं जो प्रोग्राम के डाटा फ्लो को नियंत्रित करते हैं। वैकल्पिक रूप से, लॉग फाइल्स या डीबगर्स का भी उपयोग कर सकते हैं। इम्प्लिमेंटेशन डिटेल्स में, `Console.Error` एक `TextWriter` ऑब्जेक्ट है जो `System.IO` नेमस्पेस में परिभाषित है और जिसे stderr स्ट्रीम से मैप किया गया है।

## See Also (और भी जानकारी)
- [.NET documentation on TextWriter](https://docs.microsoft.com/en-us/dotnet/api/system.io.textwriter?view=net-6.0)
- [Microsoft guide to console I/O](https://docs.microsoft.com/en-us/dotnet/standard/io)

---
title:                "मानक त्रुटि में लिखना"
date:                  2024-01-19
html_title:           "Arduino: मानक त्रुटि में लिखना"
simple_title:         "मानक त्रुटि में लिखना"

category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Writing to standard error (stderr) में हम एरर मैसेजेज को कंसोल पर प्रिंट करते हैं। Programmers इसे इसलिए इस्तेमाल करते हैं ताकि वे एरर्स को रेगुलर आउटपुट से अलग कर सकें और लॉगिंग तथा डिबगिंग में आसानी हो।

## How to: (कैसे करें:)
```kotlin
fun main() {
    // नॉर्मल मैसेज
    println("Hello, Standard Output!")
    
    // एरर मैसेज
    System.err.println("Warning: This is a standard error message!")
}
```
Sample Output:
```
Hello, Standard Output!
Warning: This is a standard error message!
```

## Deep Dive (गहराई में जानकारी):
ऐतिहासिक रूप से, stderr UNIX सिस्टम्स पर output streams का एक हिस्सा है जो terminal पर डायरेक्ट होता है। इसके alternatives हैं जैसे फाइल में लोग करना, या logging frameworks का उपयोग करना। Kotlin में `System.err` Java के `System.err` से ही derived है, जो `PrintStream` ऑब्जेक्ट होता है।

## See Also (संबंधित जानकारी):
- [Kotlin Official Documentation](https://kotlinlang.org/docs/reference/)
- [Java PrintStream Documentation](https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html)
- [Unix Standard Streams](https://en.wikipedia.org/wiki/Standard_streams)

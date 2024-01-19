---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या & क्यों?

Debug आउटपुट प्रिंट करना एक प्रोग्रामिंग कार्य है जिसमें हम लॉग और डिबग मेसेजों को कंसोल में प्रिंट करते हैं। यह डेवलपर्स के द्वारा उनके कोड के व्यवहार की जांच करने के लिए किया जाता है।

## कैसे करें:

तहत कुछ प्रमुख कोडिंग उदाहरण दी गई हैं।

```C#
using System;
using System.Diagnostics;

public class DebugExample 
{
    public static void Main() 
    {
        Debug.WriteLine("Debug information");
        Debug.Indent();
        Debug.WriteLine("Inside the Indent");
        Debug.Unindent();
        Debug.WriteLine("Outside the Indent");
    }
}
```
यहां पर `Debug.WriteLine` डीबगिंग संदेश को कंसोल पर प्रिंट करेगा। `Debug.Indent` और `Debug.Unindent` की मदद से हम समझ सकते हैं कि कोड का कौन सा हिस्सा कौन से स्कोप में है।

## गहरा दौरा:

प्रिंटिंग डीबग आउटपुट कैसे काम करता है और इसका इतिहास किस प्रकार है, इसकी जांच करने का समय है। 

डीबग लाइनें केवल डीबग बिल्ड संस्करण में दिखती हैं, इसलिए ये आपकी प्रोडक्शन कोड के प्रदर्शन को प्रभावित नहीं करतीं। 

इसका विकल्प `Trace` हो सकता है जो डीबग और रिलीज संस्करण में दोनों प्रिंट होता है। 

हम बदल सकते हैं की डीबग संदेश आउटपुट कहाँ जाएगा विशेष trace listeners की मदद से।.

## और देखें:

- Microsoft Documentation: Debug Class (https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug)
- Microsoft Documentation: Trace Listeners (https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.tracelistener)
- StackOverflow: Debugging & Tracing in C# (https://stackoverflow.com/questions/577411/how-can-i-debug-and-trace-in-c-sharp)
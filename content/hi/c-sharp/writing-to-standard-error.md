---
title:                "मानक त्रुटि के लिए लिखना"
date:                  2024-02-03T19:34:04.021672-07:00
model:                 gpt-4-0125-preview
simple_title:         "मानक त्रुटि के लिए लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
C# में स्टैंडर्ड एरर (stderr) में लिखना यह सुनिश्चित करता है कि त्रुटि संदेश और निदानों को सामान्य आउटपुट (stdout) से अलग निर्देशित किया जा सके ताकि उपयोगकर्ता और विकासक नियमित कार्यक्रम आउटपुट और त्रुटि सूचनाओं के बीच अंतर कर सकें। प्रोग्रामर्स इसे डिबगिंग और लॉगिंग को अधिक कुशल बनाने के लिए करते हैं, जिससे अनुप्रयोगों का संचालन और रखरखाव अधिक सुगम हो जाता है।

## कैसे करें:
C# में, स्टैंडर्ड एरर में लिखना `Console.Error` स्ट्रीम का उपयोग करके हासिल किया जा सकता है। यह स्ट्रीम विशेष रूप से त्रुटि संदेशों और निदानों के लिए इस्तेमाल की जाती है। यहाँ एक मूल उदाहरण है:

```csharp
Console.Error.WriteLine("Error: Failed to process the request.");
```

नमूना आउटपुट (stderr के लिए):
```
Error: Failed to process the request.
```

जहाँ आप एक तृतीय-पक्ष पुस्तकालय का उपयोग कर रहे हों जो उन्नत लॉगिंग क्षमताओं की पेशकश करता है, जैसे कि `Serilog` या `NLog`, आप इन पुस्तकालयों को stderr पर त्रुटि लॉग्स लिखने के लिए कॉन्फ़िगर कर सकते हैं। जबकि ये उदाहरण साधारण कंसोल रीडायरेक्शन पर केंद्रित हैं, याद रखें कि उत्पादन अनुप्रयोगों में, लॉगिंग फ्रेमवर्क अधिक मजबूत त्रुटि हैंडलिंग और आउटपुट विकल्प प्रदान करते हैं। यहाँ `Serilog` के साथ एक साधारण उदाहरण है:

पहले, Serilog पैकेज और इसके Console sink को इंस्टॉल करें:

```
Install-Package Serilog
Install-Package Serilog.Sinks.Console
```

फिर, Serilog को stderr में लिखने के लिए कॉन्फ़िगर करें:

```csharp
using Serilog;

Log.Logger = new LoggerConfiguration()
    .WriteTo.Console(standardErrorFromLevel: Serilog.Events.LogEventLevel.Error)
    .CreateLogger();

Log.Information("This is a normal message.");
Log.Error("This is an error message.");
```

नमूना आउटपुट (त्रुटि संदेश के लिए stderr में):
```
[15:04:20 ERR] This is an error message.
```

नोट: Serilog के कंसोल सिंक में `standardErrorFromLevel` कॉन्फ़िगरेशन सभी लॉग इवेंट्स को निर्दिष्ट स्तर (इस मामले में त्रुटि) या उससे ऊंचे स्तर पर स्टैंडर्ड एरर स्ट्रीम में रीडायरेक्ट करता है, जबकि निम्न स्तर के संदेश जैसे कि सूचना स्टैंडर्ड आउटपुट स्ट्रीम में लिखे जाते हैं।

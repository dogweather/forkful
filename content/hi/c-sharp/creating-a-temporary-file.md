---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "Arduino: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# अस्थाई फ़ाइलें C# में कैसे उत्पन्न करें?

## क्या और क्यों?
एक अस्थाई फ़ाइल बनाना एक सामान्य कार्य होता है जो डेटा को कुछ समय के लिए संग्रहीत करता है। प्रोग्रामर इसे द्वितीयक संग्रहण, उपयोगकर्ता सत्र, डेटा संकलन और तात्कालिक कार्यवाही के उद्देश्यों के लिए उत्पन्न करते हैं।

## कैसे उत्पन्न करें:
```C#
Using System.IO;
Class Example 
{
  static void Main() 
  {
    string tempPath = Path.GetTempPath();
    string tempFileName = Path.GetRandomFileName();
    string tempFile = Path.Combine(tempPath, tempFileName);

    File.WriteAllText(tempFile, "यह एक अस्थायी फ़ाइल है।");

    Console.WriteLine(tempFile);
    Console.WriteLine(File.ReadAllText(tempFile));
  }
}
```
इस कोड के चलते, एक अस्थायी फ़ाइल उत्पन्न होती है, जिसमें "यह एक अस्थायी फ़ाइल है।" लिखा होता है। फ़ाइल का पथ और उसकी सामग्री कंसोल पर दिखाई देती है।

## गहराई से जानकारी
- इतिहासिक प्रसंग: अस्थायी फ़ाइलें सर्वप्रथम Unix ऑपरेटिंग सिस्टम में प्रयोग होनी शुरू हुई थीं जो तात्कालिक डेटा संग्रहण के उद्देश्यों के लिए डिस्क पर स्थान उत्पन्न करती थीं।
- विकल्प: अस्थायी डेटाबेस, जैसे कि SQLite, या आपके एप्लिकेशन की मेमोरी का उपयोग कर सकते हैं, लेकिन फ़ाइलों का उपयोग करना तभी बेहतर होता है जब डेटा बीच-मैं लॉस हो सकता है।
- क्रियान्वयन विवरण: `Path.GetTempPath()` और `Path.GetRandomFileName()` C# की `System.IO` नेमस्पेस का हिस्सा है, जो विभिन्न फ़ाइल और निर्देशिका संबंधी कार्यों को निष्पादित करने के लिए उपयोग की जाती हैं।

## अन्य संसाधनों के लिंक
- [C# में फ़ाइल संचालन पर एमडीएन](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=net-5.0)
- [C# में पाथ संचालन पर एमडीएन](https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=net-5.0)
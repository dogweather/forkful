---
title:                "अस्थायी फाइल बनाना"
date:                  2024-01-20T17:41:42.582142-07:00
model:                 gpt-4-1106-preview
simple_title:         "अस्थायी फाइल बनाना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
टेम्परेरी फाइल बनाना मतलब एक ऐसी फाइल जिसे अस्थायी काम के लिए बनाया जाता है और फिर उसे हटा दिया जाता है। प्रोग्रामर्स ये इसलिए करते हैं ताकि कुछ डाटा को अस्थायी रूप से स्टोर कर सकें या प्रोसेसिंग के दौरान किसी इंटरमीडिएट स्टेप के लिए।

## कैसे करें:
```PowerShell
# एक टेम्परेरी फाइल बनाने का कोड
$tempFile = [System.IO.Path]::GetTempFileName()
# टेम्प फाइल में कुछ लिखें
"यह एक परीक्षा डाटा है" | Out-File -FilePath $tempFile
# टेम्प फाइल का डाटा पढ़ें
Get-Content -Path $tempFile
# टेम्प फाइल हटाएं
Remove-Item -Path $tempFile
```

उदाहरण के चलते एक टेम्प फाइल बनेगी, उसमें डाटा लिखेंगे, फिर डाटा पढ़ेंगे, और आखिर में फाइल को हटा देंगे।

## गहराई से जानिए:
टेम्परेरी फाइलें ऑपरेटिंग सिस्टम के प्रारम्भिक दिनों से उपयोग में हैं। `GetTempFileName()` मेथड नेट फ्रेमवर्क से आता है, जो कि सुविधाजनक रूप से सेफ और प्रेडिक्टेबल तरीके से एक यूनीक टेम्प फाइल जनरेट करता है। वैकल्पिक तरीकों में `$env:TEMP` पर्यावरण वेरिएबल का इस्तेमाल करना, या फिर `[System.IO.Path]::GetRandomFileName()` के जरिए एक रैन्डम नाम की फाइल बनाना शामिल है, लेकिन ये ऑटोमेटिक फाइल पाथ नहीं देता। टेम्प फाइल बनाते वक्त याद रखें कि फाइल मैनेजमेंट जिम्मेदारी से करें और उन्हें हटाना न भूलें ताकि सिस्टम प्रदूषित न हो।

## और भी जानकारी के लिए:
- [System.IO.Path Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=net-7.0)
- [PowerShell Gallery Scripts for File Management](https://www.powershellgallery.com/packages?q=File+Management)

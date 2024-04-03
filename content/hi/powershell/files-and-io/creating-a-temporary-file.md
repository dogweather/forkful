---
date: 2024-01-20 17:41:42.582142-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: ."
lastmod: '2024-03-13T22:44:52.735989-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093E\u0907\u0932 \u092C\
  \u0928\u093E\u0928\u093E"
weight: 21
---

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

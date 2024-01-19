---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

पावरशेल में 'वर्तमान दिनांक' पाने का अर्थ है किसी विशेष समय पर वर्तमान दिनांक और समय का उपयोग करना। प्रोग्रामर इसे डेटा-ड्रिवन कार्य, लोग्ज़ को स्टाम्प करने, और समय-संवेदी कार्यवाही को समर्थन देने के लिए करते हैं।

## कैसे:

वर्तमान दिनांक प्राप्त करने के लिए आप एक 'Get-Date' cmdlet का उपयोग कर सकते हैं:

```PowerShell
$CurrentDate = Get-Date
Write-Host $CurrentDate
```

उत्तर:

```PowerShell
Saturday, 22 January 2022 10:51:26
```

इसे स्वरूपित भी किया जा सकता है:

```PowerShell
$CurrentDate = Get-Date -Format "yyyy-MM-dd"
Write-Host $CurrentDate
```

उत्तर:

```PowerShell
2022-01-22
```

## गहराईसे ज्ञान:

**ऐतिहासिक संदर्भ**: PowerShell, Microsoft द्वारा सिस्टम प्रबंधन के लिए विकसित किया गया एक शक्तिशाली शेल-स्क्रिप्टिंग भाषा है, जिसमें 'Get-Date' cmdlet 1.0 से ही शामिल है। 

**विकल्प**: अन्य भाषाओं के मुकाबले, PowerShell के 'Get-Date' cmdlet का उपयोग करना आसान है। लेकिन आप .NET के 'DateTime' ऑब्जेक्ट का भी उपयोग कर सकते हैं।

```PowerShell
$CurrentDate = [DateTime]::Now
Write-Host $CurrentDate
```

**कार्यान्वयन विवरण**: 'Get-Date' cmdlet एक .NET 'DateTime' ऑब्जेक्ट वापस करता है। इसका अर्थ है कि आप इससे 'DateTime' विधियों और गुणों का उपयोग कर सकते हैं।

## और भी लिंक:

- [Get-Date ऑफिसियल डॉक्युमेंटेशन](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [डेट और समय के फॉर्मेट](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
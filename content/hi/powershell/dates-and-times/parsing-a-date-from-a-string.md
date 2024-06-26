---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:01.176176-07:00
description: "\u0915\u0948\u0938\u0947: PowerShell \u0905\u092A\u0928\u0947 `Get-Date`\
  \ cmdlet \u0914\u0930 `[datetime]` type \u090F\u0915\u094D\u0938\u0947\u0932\u0947\
  \u0930\u0947\u091F\u0930 \u0915\u0947 \u0938\u093E\u0925 \u0938\u094D\u091F\u094D\
  \u0930\u093F\u0902\u0917 \u0938\u0947 \u0924\u093E\u0930\u0940\u0916 \u092A\u093E\
  \u0930\u094D\u0938 \u0915\u0930\u0928\u093E \u0938\u0930\u0932 \u092C\u0928\u093E\
  \u0924\u093E \u0939\u0948, \u091C\u094B \u0915\u093F \u092E\u093E\u0928\u0915 \u0924\
  \u093F\u0925\u093F \u092A\u094D\u0930\u093E\u0930\u0942\u092A\u094B\u0902 \u0915\
  \u0947\u2026"
lastmod: '2024-04-05T21:53:54.681866-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u0905\u092A\u0928\u0947 `Get-Date` cmdlet \u0914\u0930 `[datetime]`\
  \ type \u090F\u0915\u094D\u0938\u0947\u0932\u0947\u0930\u0947\u091F\u0930 \u0915\
  \u0947 \u0938\u093E\u0925 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\
  \u0947 \u0924\u093E\u0930\u0940\u0916 \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\
  \u0928\u093E \u0938\u0930\u0932 \u092C\u0928\u093E\u0924\u093E \u0939\u0948, \u091C\
  \u094B \u0915\u093F \u092E\u093E\u0928\u0915 \u0924\u093F\u0925\u093F \u092A\u094D\
  \u0930\u093E\u0930\u0942\u092A\u094B\u0902 \u0915\u0947 \u0932\u093F\u090F \u0905\
  \u091A\u094D\u091B\u0940 \u0924\u0930\u0939 \u0938\u0947 \u0915\u093E\u092E \u0915\
  \u0930\u0924\u093E \u0939\u0948\u0964 \u091C\u091F\u093F\u0932 \u092F\u093E \u0917\
  \u0948\u0930-\u092E\u093E\u0928\u0915 \u0924\u093F\u0925\u093F \u0938\u094D\u091F\
  \u094D\u0930\u093F\u0902\u0917 \u0915\u0947 \u0932\u093F\u090F, `[datetime]::ParseExact`\
  \ \u0935\u093F\u0927\u093F \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\
  \u0915\u0947 \u0938\u091F\u0940\u0915 \u092A\u094D\u0930\u093E\u0930\u0942\u092A\
  \ \u0928\u093F\u0930\u094D\u0926\u093F\u0937\u094D\u091F \u0915\u093F\u092F\u093E\
  \ \u091C\u093E \u0938\u0915\u0924\u093E \u0939\u0948\u0964."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0924\u093E\
  \u0930\u0940\u0916 \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u093E"
weight: 30
---

## कैसे:
PowerShell अपने `Get-Date` cmdlet और `[datetime]` type एक्सेलेरेटर के साथ स्ट्रिंग से तारीख पार्स करना सरल बनाता है, जो कि मानक तिथि प्रारूपों के लिए अच्छी तरह से काम करता है। जटिल या गैर-मानक तिथि स्ट्रिंग के लिए, `[datetime]::ParseExact` विधि का उपयोग करके सटीक प्रारूप निर्दिष्ट किया जा सकता है।

### `Get-Date` और `[datetime]` का उपयोग करना:
```powershell
# Get-Date का उपयोग करके सरल रूपांतरण
$stringDate = "2023-04-01"
$date = Get-Date $stringDate
echo $date
```
**नमूना आउटपुट:**
```
शनिवार, अप्रैल 1, 2023 12:00:00 AM
```

```powershell
# टाइप एक्सेलेरेटर [datetime] का उपयोग करना
$stringDate = "April 1, 2023"
$date = [datetime]$stringDate
echo $date
```
**नमूना आउटपुट:**
```
शनिवार, अप्रैल 1, 2023 12:00:00 AM
```

### गैर-मानक प्रारूपों के लिए `[datetime]::ParseExact` का उपयोग करना:
स्वतः पहचाने जाने वाले प्रारूपों के बाहर, आप सही पार्सिंग सुनिश्चित करने के लिए सटीक प्रारूप निर्धारित कर सकते हैं।
```powershell
$stringDate = "01-04-2023 14:00"
$format = "dd-MM-yyyy HH:mm"
$culture = [Globalization.CultureInfo]::InvariantCulture
$date = [datetime]::ParseExact($stringDate, $format, $culture)
echo $date
```
**नमूना आउटपुट:**
```
शनिवार, अप्रैल 1, 2023 2:00:00 PM
```

### तृतीय-पक्ष पुस्तकालयों का उपयोग करना
हालांकि PowerShell स्वयं तारीख पार्सिंग के लिए काफी शक्तिशाली है, बहुत जटिल परिदृश्यों या अतिरिक्त कार्यक्षमता के लिए, आप .NET पुस्तकालयों जैसे कि NodaTime की खोज कर सकते हैं, हालांकि कई विशिष्ट उपयोग के मामलों के लिए, PowerShell की मूल क्षमताएं पर्याप्त होंगी।

```powershell
# NodaTime का उपयोग केवल एक चित्रण के रूप में, ध्यान दें कि आपको पुस्तकालय को अपनी परियोजना में जोड़ने की आवश्यकता है
# Install-Package NodaTime -Version 3.0.5
# एक तारीख पार्स करने के लिए NodaTime का उपयोग करना
[string]$stringDate = "2023-04-01T14:00:00Z"
[NodaTime.Instant]::FromDateTimeUtc([datetime]::UtcNow)
[NodaTime.LocalDate]$localDate = [NodaTime.LocalDate]::FromDateTime([datetime]::UtcNow)
echo $localDate
```
**नमूना नोट:** उपरोक्त कोड एक अवधारणात्मक चित्रण है। व्यवहार में, सुनिश्चित करें कि NodaTime प्रकारों और विधियों को उपलब्ध कराने के लिए आपकी परियोजना में सही ढंग से जोड़ा गया है।

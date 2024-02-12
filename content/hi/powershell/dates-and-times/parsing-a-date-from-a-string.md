---
title:                "स्ट्रिंग से तारीख पार्स करना"
date:                  2024-02-03T19:16:01.176176-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग से तारीख पार्स करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
एक स्ट्रिंग से तिथि पार्स करना इस बात के बारे में है कि कैसे लिखित तारीखों को पाठ रूप में पहचाना जाए और उन्हें एक तिथि डेटा प्रकार में बदला जाए जिसे PowerShell समझ सके और इसके साथ काम कर सके। प्रोग्रामर इसे तिथियों को संभालने, प्रारूपित करने, तुलना करने, या गणना करने के लिए करते हैं, जो कि लॉग फ़ाइलों, उपयोगकर्ता इनपुट, या डेटा प्रोसेसिंग से संबंधित स्क्रिप्ट में आम कार्य हैं।

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

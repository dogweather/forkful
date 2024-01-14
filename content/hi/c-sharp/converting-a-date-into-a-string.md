---
title:                "C#: एक तारीख को स्ट्रिंग में बदलना"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्यों

किसी भी एक्सपर्ट के लिए, दिनांक को स्ट्रिंग में बदलना सामान्य लग सकता है, लेकिन एक नए उपयोगकर्ता के लिए यह अपेक्षित से कठिन हो सकता है। दिनांक को स्ट्रिंग में बदलने में अनावश्यक गलतियों की संख्या कम करने के लिए, हम इस प्रकार के अनुभावों को साझा कर रहे हैं जो आपको सही रास्ता दिखा सकते हैं।

## कैसे

दिनांक को स्ट्रिंग में बदलने के लिए, हम DateTime.ToString() फ़ंक्शन का उपयोग कर सकते हैं। इसके लिए हमें आगे दिए गए स्टेटमेंट को इम्प्लीमेंट करना होगा:

```C#
DateTime date = DateTime.Now;
string dateString = date.ToString("MM/dd/yyyy");
```

यह मैथड्स में 3 मार्च 2020 की तारीख को "03/03/2020" के रूप में स्ट्रिंग में बदल देगा।

## डीप डाइव

दिनांक स्ट्रिंग में बदलने के लिए ज्यादातर उपयोग हमेशा तारीख स्वरूप पर निर्भर करता है। कुछ प्रारूप जैसे "MM/dd/yyyy" (महीना/दिन/साल), "dd MMM yyyy" (दिनांक महीना साल), "yyyy-MM-dd" (साल माह दिन) आदि प्रचलित हैं। आप इन प्रारूपों को अपनी आवश्यकतानुसार शैलीबद्ध कर सकते हैं। इसके अलावा, आप विशेष पैटर्न भी प्रयोग कर सकते हैं जैसे "ddd" जो तीन अक्षरों वाले दिन का नाम वापस देगा।

## देखें भी

- [DateTime Structure - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1)
- [Standard Date and Time Format Strings - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings)
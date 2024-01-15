---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों

वर्तमान तारीख हासिल करने के लिए कोई मुख्य उपयोग है। एक अन्य सम्भावना है कि प्रोग्रामर अपने कोड में दिनांक की जानकारी को स्टोर करें या उसे अलग भाषाओं में प्रदर्शित करने के लिए इस्तेमाल करें।

## कैसे करें

```C#
DateTime currentDate = DateTime.Today; 
string formattedDate = currentDate.ToString("dd MMMM yyyy"); 
Console.WriteLine("आज की तारीख: " + formattedDate); 

// आज की तारीख: २५ अप्रैल २०२१
```

## गहराई में जाएं

DateTime डेटा टाइप C# में वर्तमान दिनांक को रखने के लिए उपयोग किया जाता है। इसके अलावा, DateTime ऑब्जेक्ट को मंगलवार, गुरुवार, शुक्रवार आदि जैसे वे दिनों के नाम भी प्रदर्शित करने के लिए उपयोग किया जा सकता है। यह एक सुविधा है जो दिनांक को कुछ अलग तरीके से प्रदर्शित करने में मदद करती है।

## इसे भी देखें

- [C# DateTime डेटा टाइप](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
- [C# में दिनांक को स्टोर करने का तरीका](https://www.c-sharpcorner.com/UploadFile/mahesh/c-sharp-datetime-conversion-functions/)
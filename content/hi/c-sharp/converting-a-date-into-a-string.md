---
title:                "C#: तारीख को एक स्ट्रिंग में बदलना"
simple_title:         "तारीख को एक स्ट्रिंग में बदलना"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## लोग इस प्रोग्रामिंग का उपयोग क्यों करते हैं? 

दिनांक को स्ट्रिंग में रूपांतरित करने से लेकर लोगों को दिनांक के साथ काम करने में सुविधा होती है।

## कैसे करें

```C#
//उदाहरण 1: वर्तमान दिनांक को लंबा दिनांक स्ट्रिंग में रूपांतरित करें
DateTime current = DateTime.Now;
string dateString = current.ToString("D");
Console.WriteLine(dateString);
// प्रदर्शित होने वाला आउटपुट: गुरुवार, 31 दिसंबर 2020

//उदाहरण 2: डेट फॉर्मेट को स्वचालित रूप से परिवर्तित करें
DateTime input = new DateTime(2020, 12, 31);
string output = input.ToShortDateString();
Console.WriteLine(output);
// प्रदर्शित होने वाला आउटपुट: 31/12/2020
```

## गहराई तक जाओ

दिनांक को स्ट्रिंग में रूपांतरित करने का शास्त्रीय तरीका ```ToString()``` में समझाया गया है जो दिनांक को अलग-अलग तारीख फॉर्मेटों में रूपांतरित करने में मदद करता है। यह मेथड दो प्रमाणों को स्वीकार करता है - पहला प्रमाण दिनांक का दृश्य है और दूसरा प्रमाण दिनांक फॉर्मेट है जिसमें दिनांक को रूपांतरित किया जाएगा। लेकिन इस उदाहरण में, हमने तो बस एक ही प्रमाण का उपयोग किया है जो दिनांक को लंबा स्ट्रिंग में रूपांतरित कर देता है।

## देखें भी

[MSDN के डेट फंक्शन्स कैनेगोरी](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings)

[कैसे किसी भी दिनांक को रूपांतरित करें?](https://www.geeksforgeeks.org/how-to-format-a-date-in-c-sharp/)
---
title:                "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों

अनुक्रम का बेस आज का तारीख प्राप्त करने में हमें क्यों रुचि है? यह एक असाधारण चीज़ हो सकती है, लेकिन यह आपके कोड में उचित तारीख और समय को ले जाने में बहुत महत्वपूर्ण हो सकता है। इसके साथ हम प्रक्रियाओं को ट्रैक कर सकते हैं और अपनी एप्लिकेशन को उदयापन की विशेषताओं से लाभानुवर्ती बना सकते हैं।

## कैसे

```C#
// Using DateTime class
DateTime currentDate = DateTime.Now; // get current date and time
Console.WriteLine(currentDate); // output: 5/25/2021 10:30:00 AM
```

यहां हमने `DateTime` कक्षा का उपयोग करके `DateTime.Now` गुण का उपयोग करके वर्तमान तारीख और समय को प्राप्त किया है। यह प्रक्रिया आपको वर्तमान तारीख के सटीक रूप से उपयोग को सुनिश्चित करता है और हमारे काम में आता है। आप इसे विजेट के साथ मूल्यों को संग्रहित करने के लिए और अपने विजेट को परिवर्तित करने के लिए भी उपयोग कर सकते हैं।

```C#
// Using DateTime.Now property
DateTime currentDate = DateTime.Now.Date; // get current date
Console.WriteLine(currentDate); // output: 5/25/2021 12:00:00 AM
```

अगर आपको सिर्फ वर्तमान तारीख की जरूरत है, तो आप `DateTime.Now` की जगह `DateTime.Now.Date` को उपयोग कर सकते हैं। यह आपको वर्तमान तारीख के मुख्य अंश को लेने में मदद करता है।

## गहराई से जाएं

जब हम किसी विशेषता का उपयोग करते हैं, तो हमें जानने की जरूरत होती है कि वह बात आखिर हो क्या रही है। `DateTime.Now` के अलावा, हम `DateTime.Today` और `DateTime.UtcNow` भी प्राप्त कर सकते हैं। इन गुणको लेने से
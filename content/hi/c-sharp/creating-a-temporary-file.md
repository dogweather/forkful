---
title:                "कंप्यूटर प्रोग्रामिंग में अस्थायी फाइल बनाना"
html_title:           "C#: कंप्यूटर प्रोग्रामिंग में अस्थायी फाइल बनाना"
simple_title:         "कंप्यूटर प्रोग्रामिंग में अस्थायी फाइल बनाना"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें अपने कोड में अस्थायी फाइलें बनाने की आवश्यकता होती है, जो कुछ समय के लिए ही हमारी जरूरत पूरी करती है। इस लेख में, हम जानेंगे कि C# में अस्थायी फाइलें कैसे बनाई जा सकती हैं और उनसे कैसे उपयोग किया जा सकता है। 

## कैसे करें

अस्थायी फाइलें बनाने के लिए, हमें एक नया `FileStream` ऑब्जेक्ट बनाना होगा। इसके लिए हम `using` की घोषणा के साथ `FileStream` को इनिशियलाइज़ कर सकते हैं: 

```C#
using (FileStream fs = new FileStream("temp.txt", FileMode.Create))
{
    // code to write to the file
}
```

यहां "temp.txt" हमारी अस्थायी फाइल का नाम है और `FileMode.Create` हमें फाइल को लिखने के लिए ओपन करता है। इसके बाद हम फाइल में लिखने के लिए `StreamWriter` का उपयोग कर सकते हैं:

```C#
using (FileStream fs = new FileStream("temp.txt", FileMode.Create))
{
    using (StreamWriter writer = new StreamWriter(fs))
    {
        writer.WriteLine("This is a temporary file created in C#");
    }
}
```

अब यह फाइल `temp.txt` बन जाएगी और हम उसे फाइल सिस्टम में उपयोग कर सकते हैं। 

## गहराई में जाएं

`FileStream` के साथ `using` ब्लॉक का उपयोग करने से हम आपरेशन के अंत में श्रोता को स्वतः ही बंद कर देते हैं। इससे हमें एक नया फाइल सिस्टम हैंडल लेने की आवश्यकता नहीं होती है। इसके अलावा, हम `StreamWriter` का भी उपयोग कर सकते हैं जो हमें लिखने के लिए अधिक अनुकूलता देता है। `StreamWriter` को हम `using` ब्लॉक में भी शामिल कर सकते हैं, लेकिन उससे फाइल हैंडल बंद नहीं होता। इसलिए, यदि हमें `StreamWriter` का उ
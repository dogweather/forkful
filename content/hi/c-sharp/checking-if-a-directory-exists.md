---
title:                "डायरेक्टरी मौजूद है या नहीं जाँचना"
html_title:           "C#: डायरेक्टरी मौजूद है या नहीं जाँचना"
simple_title:         "डायरेक्टरी मौजूद है या नहीं जाँचना"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
डायरेक्टरी मौजूद है या नहीं, का जांचने का अर्थ है कि प्रोग्राम एक विशेष फ़ाइल पथ हेतु अस्तित्व की पुष्टि करता है। प्रोग्रामर्स इसे तब इस्तेमाल करते हैं जब उन्हें यह सुनिश्चित करना होता है कि एक ऑपरेशन करने से पहले एक डायरेक्टरी मौजूद है।

## कैसे करें:
C# में किसी डायरेक्टरी की उपस्थिति की जांच करने के लिए, आप `Directory.Exists` फ़ंक्शन का उपयोग कर सकते हैं। यह जांचता है कि निर्दिष्ट पथ पर डायरेक्टरी है या नहीं।

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string path = @"C:\SomeDirectory";
        if (Directory.Exists(path))
        {
            Console.WriteLine("Directory exists.");
        }
        else
        {
            Console.WriteLine("Directory does not exist.");
        }
    }
}
```

संभाव्य आउटपुट:

```
Directory does not exist.
```

## गहरी डाइव
इतिहासिक प्रसंग में, इस तकनीक का उपयोग करने से पहले आपको निर्देशिका का पूर्ण पथ सही होना चाहिए। अन्य विकल्पों में, आप `DirectoryInfo.Exists` फ़ंक्शन का उपयोग कर सकते हैं, लेकिन यह Directory class से अधिक overhead ला सकता है। इन फंक्शनों का उपयोग जांचने के लिए किया जाता है कि क्या फ़ाइल सिस्टम निर्देशिका पथ के सहज उपयोग के लिए सही है।

## देखें भी
1. माइक्रोसॉफ्ट डॉक्स: [डायरेक्टरी. मौजूदता की जांच करें](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists?view=net-5.0)
2. StackOverflow: [कैसे निर्दिष्टित किया जाए कि क्या एक निर्देशिका मौजूद हैं](https://stackoverflow.com/questions/1395205/better-way-to-check-if-a-path-is-a-file-or-a-directory)
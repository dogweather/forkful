---
title:                "C#: स्टैंडर्ड त्रुटि में लिखना"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# क्यों

क्या आपने कभी सोचा है कि कंप्यूटर पर लिखने के लिए स्टैंडर्ड त्रुटि का उपयोग क्यों किया जाता है? स्टैंडर्ड त्रुटि का उपयोग करने से हम अपनी प्रोग्रामिंग में त्रुटियां ढूंढ़ने और सुधारने में सक्षम होते हैं। इस तरीके से हम अपने कोड को मजबूत बनाते हैं और अधिक सुरक्षित बनाते हैं।

# कैसे करें

```C#
// कोड देखें कि कैसे हम स्टैंडर्ड त्रुटि का उपयोग कर सकते हैं
using System;

class Program
{
    static void Main()
    {
        Console.Error.WriteLine("यहां कुछ त्रुटि संदेश होते हैं।");
    }
}

// क़तार में संदेश प्रिंट होते हैं
यहां कुछ त्रुटि संदेश होते हैं।
```

# गहराई में जाएं

स्टैंडर्ड त्रुटि को कैसे उपयोग किया जाए, इसके अलावा भी कई अन्य उपयोग हैं। इसके बारे में गहराई से जानने के लिए, आप अपनी क्षमताओं को सुधार सकते हैं और सुरक्षित कोडिंग अनुभव प्राप्त कर सकते हैं। इसके साथ ही, आप संदेश को अपनी पसंद के अनुसार रूपांतरित कर सकते हैं। इस तरह से, आप अपने संदेश को अपनी गहराई और स्पष्टता से उपयोगी बना सकते हैं।

# देखें भी

- [Microsoft Documentation on Standard Error](https://docs.microsoft.com/en-us/dotnet/api/system.console.error)
- [GeeksforGeeks article on Standard Error](https://www.geeksforgeeks.org/standard-error-stream-in-c-sharp/)
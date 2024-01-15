---
title:                "एक टेक्स्ट फाइल पढ़ना"
html_title:           "C#: एक टेक्स्ट फाइल पढ़ना"
simple_title:         "एक टेक्स्ट फाइल पढ़ना"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# क्यों

कभी-कभी हमें किसी फाइल से डेटा पढ़ने की आवश्यकता होती है। जैसे कि एक प्रोग्राम में स्टोर डेटा को लोड करने के लिए या प्यारसेल डाटा को पढ़ने के लिए। इस आर्टिकल में हम आपको सुनिश्चित करेंगे कि आप एक टेक्स्ट फाइल को कैसे पढ़ सकते हैं।

## कैसे करें

```c#
StreamReader file = new StreamReader("test.txt"); 
string line;
while((line = file.ReadLine()) != null)
{
    Console.WriteLine(line); 
}
```

यहां, हमने `StreamReader` क्लास का उपयोग करके फाइल को रीड किया है। हम एक लाइन एक लाइन ऑब्जेक्ट को रीड करते हैं और उसे `Console.WriteLine()` के साथ प्रिंट करते हैं। आपके कंसोल में फाइल का सामग्री देखेंगे।

```c#
using (StreamReader file = new StreamReader("test.txt"))
{
    string line;
    while ((line = file.ReadLine()) != null)
    {
Console.WriteLine(line);
    }
}
```

यह एक अलग तरीका है जो `using` स्टेटमेंट का उपयोग करके हमें फाइल को close करने की जरूरत नहीं होती है। जब `using` ब्लॉक से बाहर निकलते हैं, वह अंतिम रूप से फाइल को close कर देता है।

## डीप डाइव

आपने ऊपर से देखा कि हमने `StreamReader` को `test.txt` नाम की फाइल से रीड किया है। यदि आपको अपनी दूसरी लोकेशन से डेटा पढ़ना होता है, तो आप फ़ाइल पथ को `StreamReader` कन्स्ट्रक्टर में पास कर सकते हैं। आप फ़ाइल को रीड किए जाने से पहले उसकी उपस्थिति को भी जांच सकते हैं।

## देखें भी

- [C# संस्करण इतिहास](https://en.wikipedia.org/wiki/C_Sharp_(programming_language)#Versions)
- [C# आम बातें](https://www.tutorialspoint.com/csharp/csharp_programming_basics.htm)
- [C# फ़ाइल ऑपरेशंस](https://www.c-sharpcorner.com/UploadFile/3d39b
---
title:                "C#: डायरेक्टरी मौजूद है कि नहीं पता करना"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्यों

कोई भी बहुत सारे उद्देश्यों के लिए अपने कंप्यूटर पर फ़ोल्डर या डायरेक्ट्री बनाता है। चाहे वह फाइलों को संग्रहित करने के लिए हो, फाइलों को सॉर्ट करने के लिए हो या अन्य किसी कोड को इस्तेमाल करने के लिए हो। इसलिए, जब हम अपना कोड लिखते हैं, हमें यह पता होना आवश्यक है कि डायरेक्ट्री का विरामांक मौजूद है या नहीं।

## कैसे करें

आप अपने C# कोड में ```Directory.Exists()``` फ़ंक्शन का इस्तेमाल करके सरलता से एक डायरेक्ट्री के मौजूद होने को जांच सकते हैं। नीचे कोड ब्लॉक में हम एक उदाहरण देखेंगे:

```C#
using System;
using System.IO;

namespace DirectoryExistsExample
{
    class Program
    {
        static void Main(string[] args)
        {
            // अगर MyFolder नामक एक डायरेक्ट्री मौजूद है तो true लौग किए जाएंगे
            bool directoryExists = Directory.Exists("MyFolder");
            Console.WriteLine(directoryExists);
            // अगर MyFolder नामक एक डायरेक्ट्री मौजूद नहीं है तो false लौग किए जाएंगे
            directoryExists = Directory.Exists("NonexistentFolder");
            Console.WriteLine(directoryExists);
        }
    }
}
```

जैसा कि आप अंतरिक्ष से लक्ष्य जोड़ सकते हैं कि हम डायरेक्ट्री नाम को किस भांति से पास कर सकते हैं या ```Directory.Exists()``` फ़ंक्शन द्वारा उसे कैसे जांचा जाता है। आप यह भी देख सकते हैं कि किस नाम की डायरेक्ट्री कोड स्टैटमेंट का उपयोग करके बनाई गई है।

## गहराई में

हमारे पास कई तरीके हैं डायरेक्ट्री की मौजूदगी की जांच करने के लिए, जैसे कि Directory.GetFiles(), Directory.GetDirectories(), और Directory.EnumerateDirectories()। ये सभी फ
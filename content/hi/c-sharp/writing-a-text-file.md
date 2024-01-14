---
title:    "C#: कंप्यूटर प्रोग्रामिंग पर एक लेख: एक टेक्स्ट फाइल लिखना"
keywords: ["C#"]
---

{{< edit_this_page >}}

## इसका कारण

क्या आप कभी कभी एक टेक्स्ट फ़ाइल में अपने डेटा को सहेजने की ज़रूरत है? कई लोगों के लिए, यह एक आम दैनिक काम है जो कंप्यूटिंग में कुछ समय लेता है। लेकिन ऐसे मामलों में, टेक्स्ट फ़ाइल अक्सर आसान और उपयोगी समाधान हो सकती है। इस ब्लॉग पोस्ट में, हम आपको बताएंगे कि कैसे C# में एक टेक्स्ट फ़ाइल लिखा जाता है। 

## कैसे करें

टेक्स्ट फ़ाइल को C# में लिखने का सबसे सरल तरीका `StreamWriter` का उपयोग करना है। नीचे दिए गए उदाहरण में, हम एक नया `StreamWriter` बनाते हैं और फाइल का नाम और डायरेक्टरी पाठ देते हैं। फिर हम `WriteLine()` और `Write()` की मदद से अपने डेटा को फ़ाइल में लिखते हैं। 

```C#
using System;
using System.IO;

namespace writing_text_file
{
  class Program
  {
    static void Main(string[] args)
    {
      // Create a new StreamWriter and give it a filename and directory path
      // एक नया StreamWriter बनाएं और उसे फ़ाइल का नाम और डायरेक्टरी पथ दें
      using (StreamWriter writer = new StreamWriter(@"C:\Users\Username\Desktop\example.txt"))
      {
        // Write a line of text to the file and add a new line
        // फाइल में एक लाइन टेक्स्ट लिखें और एक नई लाइन जोड़ें
        writer.WriteLine("Hello Hindi readers!");
        // Write text to the file
        // फाइल में टेक्स्ट लिखें
        writer.Write("Welcome to my blog post.");
      }
    }
  }
}
```

जब आप यह कोड चलाएंगे, तो एक `example.txt` फ़ाइल बनाई जाएगी और उसमे दिए गए डेटा शामिल होगा। आप अपनी फ़ाइल को सहेजने के बाद उसे `using` की मदद से प्रक्रिया से बाहर भेज सकते हैं। 

## डीप डाइव

`StreamWriter` को `using` के साथ सं
---
title:    "C#: एक पाठ फ़ाइल लिखना"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

कोई भी कंप्यूटर या मोबाइल डिवाइस देखें तो हार्डवेयर कोई न कोई तो पेरिफेरल होता है, जैसे की डिस्क ड्राइव जिसमे आप सभी फ़ाइलें स्टोर करते हैं। इन्हीं फ़ाइलों को पढ़ने या लिखने के लिए आपको सही तरीके से प्रोग्रामिंग की जानकारी होनी चाहिए। इस लेख में हम आपको C# में टेक्स्ट फ़ाइल लिखने के लिए सही तरीके के बारे में बताएंगे।

## कैसे करें

```C#
using System; // नामस्थान संज्ञक

class Program
{
    static void Main(string[] args)
    {
        // नया फ़ाइल निर्माण करें
        string fileName = "myFile.txt";
        using (System.IO.StreamWriter file =
            new System.IO.StreamWriter(fileName))
        {
            // कुछ पंक्तियाँ लिखें
            file.WriteLine("प्रथम पंक्ति");
            file.WriteLine("दूसरी पंक्ति");
        }

        // फ़ाइल में कुछ पंक्तियाँ जोड़ें
        using (System.IO.StreamWriter file =
            new System.IO.StreamWriter(fileName, true))
        {
            file.WriteLine("तीसरी पंक्ति");
            file.WriteLine("चौथी पंक्ति");
        }

        // फ़ाइल से सभी पंक्तियाँ पढ़ें
        string[] lines = System.IO.File.ReadAllLines(fileName);
        foreach (string line in lines)
        {
            Console.WriteLine(line);
        }
    }
}
```

प्रोग्राम का आउटपुट:

```
प्रथम पंक्ति
दूसरी पंक्ति
तीसरी पंक्ति
चौथी पंक्ति
```

## गहराई

C# में टेक्स्ट फाइल लिखने के लिए, हम `System.IO` नामक नामस्थान गुना करते हैं जो में C# प्रोग्राम के लिए फ़ाइल और फ़ोल्डर को एक्सेस करने के लिए सहायता करता है। इस नामस्थान के अंतर्गत, हम `StreamWriter` नामक एक क्लास का उपयोग कर सकते हैं जो फाइल डाटा को लिखने के लिए उपयोग किया जाता है। हम `using` अभिव्यक्ति का उ
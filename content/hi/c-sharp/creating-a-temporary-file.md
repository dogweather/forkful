---
title:                "C#: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# क्यों
कभी-कभी हमें अपने काम के लिए अस्थायी फ़ाइलों की आवश्यकता होती है जो बाद में हमें नहीं चाहिए। इसलिए, हम अस्थायी फ़ाइलें बनाते हैं जो काम के बाद स्वचालित रूप से हटा दी जाती हैं। यह हमारे प्रोग्राम को सुधारने और बग्स को ठीक करने के लिए मदद करता है।

# कैसे करें
इस उदाहरण में, हम "C: \ Users \ username \ AppData \ Local \ Temp" फ़ोल्डर में अस्थायी फ़ाइल बनाने के लिए C# का उपयोग करते हुए कोड का उदाहरण देखेंगे।

```C#
using System;
using System.IO;

namespace TemporaryFiles
{
    class Program
    {
        static void Main(string[] args)
        {
            string tempFolderPath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "Temp");
            string tempFileName = "tempfile.txt";
            string tempFilePath = Path.Combine(tempFolderPath, tempFileName);

            try
            {
                using (FileStream fs = File.Create(tempFilePath))
                {
                    Console.WriteLine("फ़ाइल स्थानांतरित हो गई है।");
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("एक समस्या हुई थी - " + ex.Message);
            }
        }
    }
}
```

**आउटपुट:**

```
फ़ाइल स्थानांतरित हो गई है।
```

# गहराई में गंभीर
अस्थायी फ़ाइलें हमें टेम्पोरेरी डेटा और भी अनेक कार्यों के लिए उपयोगी साबित हो सकती हैं। हालांकि, अस्थायी फ़ाइलें न केवल सुधार के लिए होती हैं बल्कि वे सेवा निर्देशिका में मैन्युअल रूप से बनाए गए अनुकूलन अनुरोधों को पूरा करने के लिए भी उपयोगी साबित हो सकती हैं। अस्थायी फ़ाइलें डाटा प्रवाह को बढ़ावा देती हैं और समय की बचत करती हैं।

# देखें भी
- https://docs.microsoft.com/en-us/dotnet/api/system.io.file.create?view=netframework-4.8
- https://www.c-sharpcorner
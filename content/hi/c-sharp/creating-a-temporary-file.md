---
title:    "C#: तात्कालिक फाइल बनाना"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों

टेम्पररी फाइल बनाने का प्रयास क्यों किया जाता है, इसके पीछे का कारण है कि कई बार हमारे प्रोग्राम्स को किसी अलग तरीके से डेटा को स्टोर करने की जरूरत होती है। इस समस्या को हल करने के लिए हम अस्थायी फ़ाइलें बनाते हैं।

## कैसे करें

```C#
using System;
using System.IO;

namespace TemporaryFile
{
    class Program
    {
        static void Main()
        {
            // Create a temporary file with .tmp extension
            string path = Path.GetTempFileName();

            Console.WriteLine("Temporary file created at:");
            Console.WriteLine(path);

            // Write data to the temporary file
            File.WriteAllText(path, "This is a temporary file.");

            // Read data from the temporary file
            string data = File.ReadAllText(path);
            Console.WriteLine("Data written to the file:");
            Console.WriteLine(data);

            // Delete the temporary file
            File.Delete(path);
        }
    }
}
```

### Output:
Temporary file created at:
C:\Users\<username>\AppData\Local\Temp\tmpE33F.tmp
Data written to the file:
This is a temporary file.

## गहरायाई खुराक

टेम्पररी फाइल बनाने के लिए `GetTempFileName` और `GetTempPath` मेथड का उपयोग किया जाता है। इन मेथड्स की मदद से हम सिस्टम की temporary फ़ाइल डायरेक्टरी को प्राप्त करते हैं और उस फ़ाइल डायरेक्टरी में एक unique नाम के साथ एक अस्थायी फ़ाइल बनती है। इससे हमारे प्रोग्राम को अस्थायी डेटा को स्टोर करने और उसे पढ़ने की अनुमति मिलती है।

## देखें भी

- [Microsoft Docs: Creating a Temporary File in C#](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-create-a-temporary-file)
- [C# Corner: Creating Temporary Files in C#](https://www.c-sharpcorner.com/UploadFile/mahesh/temporary-file-in-C-Sharp/)
- [C# Tutorials: Temporary Files in C#](https://csharp.net-tutorials.com/advanced/temporary-files/)
---
title:                "एक टेक्स्ट फ़ाइल लिखना"
html_title:           "C#: एक टेक्स्ट फ़ाइल लिखना"
simple_title:         "एक टेक्स्ट फ़ाइल लिखना"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

# क्या और क्यों?

प्रोग्रामर एक पाठ फ़ाइल में लिखने का मतलब है कि वे एक टेक्स्ट फ़ाइल में अपने कोड का प्रवर्तन करना चाहते हैं। यह काम उन्हें अपने संगठन या अन्य प्रोजेक्ट के लिए आवश्यक होता है।

# कैसे करें?

```c#
using System.IO;

// Create a new text file
File.WriteAllText("newFile.txt", "This is a new text file.");

// Append text to an existing file
File.AppendAllText("existingFile.txt", "This text will be added to the end of the file.");

// Read text from a file
string text = File.ReadAllText("existingFile.txt");
Console.WriteLine(text); // Output: This is the text from the file.
```

# गहराई में जाएँ

आमतौर पर, प्रोग्रामर टेक्स्ट फ़ाइलों को पढ़ने और लिखने के लिए आसानी से उपलब्ध स्रोत को उपयोग करते हैं। यह पहले इंटरनेट के बाहर, शुरुआती कंप्यूटर और संगठनों द्वारा प्रयोग किया गया था। अलग रूप से, प्रोग्रामर एप्लीकेशन को डेटा को एक स्थायी फाइल में सुरक्षित रखने के लिए डेटाबेस का उपयोग कर सकते हैं। ये फ़ाइलें कंप्यूटर के लिए सर्वकार्यक्षम हैं और उसका इस्तेमाल बहुत से प्रोग्रामिंग लैंग्वेज में किया जाता है।

# अन्य स्रोत देखें

- [MSDN: Writing to a Text File](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-write-text-to-a-file)
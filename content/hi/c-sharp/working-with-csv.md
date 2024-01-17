---
title:                "CSV के साथ काम करना"
html_title:           "C#: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## त्योहार

जब हम किसी डेटा को अपने प्रोग्राम में इम्पोर्ट या एक्सपोर्ट करते हैं, तो हम CSV फाइलों के साथ काम करते हैं। आप निश्चित रूप से इन फाइलों को अपने सिस्टम में देखा होगा। ये फाइलें टेक्स्ट फाइलें होती हैं जो बड़े जातिल डेटा को स्पष्ट आकृति में प्रदर्शित करती हैं। हमारे प्रोग्रामिंग कार्यक्रमों में, हम CSV से डेटा पढ़ते और उसे लिखते हैं क्योंकि ये एक आसान और सुरक्षित तरीका है डेटा को संग्रहीत करने का।

## प्रक्रिया:

```C#
// फाइल से डेटा पढ़ें
using (StreamReader sr = new StreamReader("data.csv"))
{
    string line;
    while ((line = sr.ReadLine()) != null)
    {
        string[] data = line.Split(','); // पंक्ति को समूह में विभाजित करें
        // कोड लिखें जो डेटा जगह का मान्यांकन करता है
    }
}

// नए फाइल में डेटा लिखें
using (StreamWriter sw = new StreamWriter("new_data.csv"))
{
    // कोड लिखें जो डेटा को फाइल में योगदान करता है
}
```

## गहराई में जाएं:

CSV फाइलें 1970 के दशक में प्रवेश कर गई थीं। आज भी, इसका प्रयोग डेटा और अन्य प्रोग्राम्स के बीच डेटा साझा करने के लिए किया जाता है। हालांकि, एक प्रकार की बहुत सारी फाइल स्वरूप हैं जो अपने अंतर के आधार पर फायदेमंद हो सकते हैं। कई लोग XML फाइलों का उपयोग करते हैं जो डेटा को हीरार्कि मानचित्र में रखता है। CSV फाइलों की तुलना में, हीरार्कि मानचित्र बड़े आकार के हो सकते हैं।

## अन्य स्रोत देखें:

अगर आपको CSV फाइलों के साथ काम करने की और जानकारी चाहिए, तो आप इन लिंकों का अध्ययन कर सकते हैं।

- Microsoft Document: https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-read-text-from-a-file
- C# CSV Helper Library: https://joshclose.github.io/CsvHelper/
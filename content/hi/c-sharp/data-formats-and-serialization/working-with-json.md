---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:11.342820-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: #."
lastmod: '2024-03-13T22:44:52.368788-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 38
---

## कैसे करें:


### JSON स्ट्रिंग को ऑब्जेक्ट में पार्स करना
C# `System.Text.Json` नामस्थान प्रदान करता है जो JSON प्रक्रिया करने के लिए कुशल है। एक JSON स्ट्रिंग को C# ऑब्जेक्ट में पार्स करने के लिए, एक वर्ग को परिभाषित करें जो JSON संरचना से मेल खाता हो और `JsonSerializer.Deserialize` मेथड का उपयोग करें।

```csharp
using System;
using System.Text.Json;

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
}

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"John\", \"Age\":30}";
        Person person = JsonSerializer.Deserialize<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // आउटपुट: Name: John, Age: 30
    }
}
```

### ऑब्जेक्ट से JSON उत्पन्न करना
एक C# ऑब्जेक्ट को वापस JSON स्ट्रिंग में बदलने के लिए, `JsonSerializer.Serialize` मेथड का उपयोग करें।

```csharp
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Jane",
            Age = 25
        };

        string jsonString = JsonSerializer.Serialize(person);
        Console.WriteLine(jsonString);
        // आउटपुट: {"Name":"Jane","Age":25}
    }
}
```

### Newtonsoft.Json का उपयोग करना
`Newtonsoft.Json` (या Json.NET) एक लोकप्रिय तृतीय-पक्ष पुस्तकालय है जो JSON सीरियलाइजेशन और डिसेरियलाइजेशन के लिए अधिक लचीलेपन और विकल्प प्रदान करता है।

Json.NET का उपयोग करने के लिए, आपको पहले NuGet के माध्यम से `Newtonsoft.Json` पैकेज को स्थापित करना होगा। फिर, आप इस प्रकार एक JSON स्ट्रिंग को डिसेरियलाइज कर सकते हैं:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        string jsonString = "{\"Name\":\"Mike\", \"Age\":22}";
        Person person = JsonConvert.DeserializeObject<Person>(jsonString);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}");
        // आउटपुट: Name: Mike, Age: 22
    }
}
```

Json.NET के साथ ऑब्जेक्ट से JSON उत्पन्न करना:

```csharp
using System;
using Newtonsoft.Json;

public class Program
{
    public static void Main()
    {
        Person person = new Person
        {
            Name = "Ella",
            Age = 28
        };

        string jsonString = JsonConvert.SerializeObject(person);
        Console.WriteLine(jsonString);
        // आउटपुट: {"Name":"Ella","Age":28}
    }
}
```

ये स्निपेट C# में JSON को संभालने का त्वरित प्रारंभ देते हैं, `System.Text.Json` की निर्मित क्षमताओं और `Newtonsoft.Json` की व्यापक सुविधाओं दोनों का प्रदर्शन करते हैं।

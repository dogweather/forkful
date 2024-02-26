---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:11.342820-07:00
description: "JSON (JavaScript Object Notation) \u0915\u0947 \u0938\u093E\u0925 \u0915\
  \u093E\u092E \u0915\u0930\u0928\u093E JSON \u0921\u0947\u091F\u093E \u0915\u094B\
  \ \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u093E, \u0909\u0924\u094D\u092A\
  \u0928\u094D\u0928 \u0915\u0930\u0928\u093E \u0914\u0930 \u092A\u0942\u091B\u0924\
  \u093E\u091B \u0915\u0930\u0928\u093E \u0936\u093E\u092E\u093F\u0932 \u0939\u0948\
  , \u091C\u094B \u0906\u0927\u0941\u0928\u093F\u0915 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u093F\u0902\u0917 \u0915\u0947 \u0932\u093F\u090F \u090F\
  \u0915\u2026"
lastmod: '2024-02-25T18:49:49.567680-07:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) \u0915\u0947 \u0938\u093E\u0925 \u0915\
  \u093E\u092E \u0915\u0930\u0928\u093E JSON \u0921\u0947\u091F\u093E \u0915\u094B\
  \ \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u093E, \u0909\u0924\u094D\u092A\
  \u0928\u094D\u0928 \u0915\u0930\u0928\u093E \u0914\u0930 \u092A\u0942\u091B\u0924\
  \u093E\u091B \u0915\u0930\u0928\u093E \u0936\u093E\u092E\u093F\u0932 \u0939\u0948\
  , \u091C\u094B \u0906\u0927\u0941\u0928\u093F\u0915 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u093F\u0902\u0917 \u0915\u0947 \u0932\u093F\u090F \u090F\
  \u0915\u2026"
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

JSON (JavaScript Object Notation) के साथ काम करना JSON डेटा को पार्स करना, उत्पन्न करना और पूछताछ करना शामिल है, जो आधुनिक प्रोग्रामिंग के लिए एक महत्वपूर्ण कौशल है। यह डेटा-अदला-बदली फॉर्मेट वेब सेवाओं और APIs में अत्यधिक इस्तेमाल होता है, क्योंकि इसे पढ़ना आसान होता है और यह भाषा स्वतंत्र होती है, जिससे नेटवर्क्ड ऐप्लिकेशन पर काम कर रहे C# प्रोग्रामरों के लिए यह वेब-आधारित डेटा के साथ इंटरेक्ट करने के लिए आवश्यक हो जाती है। 

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

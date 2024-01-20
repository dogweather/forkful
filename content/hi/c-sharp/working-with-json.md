---
title:                "JSON के साथ काम करना"
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
JSON (JavaScript Object Notation) एक हल्का data interchange format है, जो आसानी से readable होता है। Programmers इसे डेटा भेजने और प्राप्त करने के लिए use करते हैं, क्योंकि यह वेब APIs के साथ seamlessly काम करता है और मल्टी-प्लैटफॉर्म सपोर्ट देता है।

## How to (कैसे करें):
आइए C# में JSON के साथ कुछ बेसिक operations को perform करें:
```C#
using System;
using System.Text.Json;

public class Program
{
    public static void Main()
    {
        // Serialize object to JSON string
        var person = new { Name = "राज", Age = 30 };
        string jsonString = JsonSerializer.Serialize(person);
        Console.WriteLine(jsonString);  // Output: {"Name":"राज","Age":30}

        // Deserialize JSON string to object
        var deserializedPerson = JsonSerializer.Deserialize<dynamic>(jsonString);
        Console.WriteLine($"{deserializedPerson.Name}, {deserializedPerson.Age}");  // Output: राज, 30
    }
}
```
हमने यहां `System.Text.Json` namespace का प्रयोग किया है।

## Deep Dive (गहराई से समझें):
JSON 2001 में Douglas Crockford द्वारा विकसित किया गया था। इसे XML का एक बेहतर विकल्प माना जाता है क्योंकि यह less verbose है और faster parse होता है। C# में, JSON के साथ काम करने के लिए `System.Text.Json` और `Newtonsoft.Json` (Json.NET) प्रमुख libraries हैं। अधिकतर cases में `System.Text.Json` preferable होता है क्योंकि यह .NET Core 3.0 में in-built है और high-performance होता है।

## See Also (और भी जानें):
- Microsoft का आधिकारिक दस्तावेज: [System.Text.Json](https://docs.microsoft.com/en-us/dotnet/api/system.text.json?view=net-6.0)
- Newtonsoft.Json library का उपयोग करते हैं: [Newtonsoft.Json](https://www.newtonsoft.com/json)
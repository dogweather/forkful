---
title:                "यामल के साथ काम करना"
date:                  2024-01-19
html_title:           "C#: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
YAML (YAML Ain't Markup Language) एक ह्यूमन-रीडेबल डेटा सीरियलाइजेशन फॉर्मेट है। प्रोग्रामर्स कॉन्फ़िग फ़ाइलें, डेटा स्टोरेज, और मैसेज ट्रांसमिशन में YAML का उपयोग करते हैं क्योंकि यह साफ, सिंपल और लैंग्वेज-न्यूट्रल है।

## How to: (कैसे?)
C# में YAML को हैंडल करने के लिए `YamlDotNet` लाइब्रेरी का इस्तेमाल किया जाता है। नीचे कोड सैंपल है:

```C#
using System;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

class Program
{
    static void Main(string[] args)
    {
        var yamlStr = @"
name: John Doe
age: 30
isProgrammer: true";

        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)  // use camel case
            .Build();

        var person = deserializer.Deserialize<Person>(yamlStr);

        Console.WriteLine($"Name: {person.Name}, Age: {person.Age}, IsProgrammer: {person.IsProgrammer}");
    }
}

public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
    public bool IsProgrammer { get; set; }
}
```

आउटपुट होगा:
```
Name: John Doe, Age: 30, IsProgrammer: True
```

## Deep Dive (विस्तृत जानकारी)
YAML सन 2001 में डेवलप किया गया था। यह JSON और XML जैसे अन्य फॉर्मेट्स की तुलना में अधिक पठनीय है। इसे .NET में इम्प्लीमेंट करने के लिए `YamlDotNet` लाइब्रेरी सबसे ज्यादा पॉपुलर है, जो डीसीरियलाइजेशन और सीरियलाइजेशन दोनों को सपोर्ट करती है। YAML में डेटा को हायरार्किकल तरीके से डेफाइन किया जाता है, जिसमें इंडेंटेशन का बहुत महत्व होता है।

## See Also (और देखें)
- YAML स्पेसिफिकेशन: https://yaml.org/spec/1.2/spec.html
- YamlDotNet गिटहब पेज: https://github.com/aaubry/YamlDotNet
- YAML और JSON की तुलना: https://en.wikipedia.org/wiki/YAML#JSON

इन लिंक्स पर जाकर आप YAML और इसके .NET इम्प्लीमेंटेशन के बारे में और अधिक जान सकते हैं।

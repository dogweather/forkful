---
title:                "C#: yaml के साथ काम करना"
simple_title:         "yaml के साथ काम करना"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्यों

हालांकि सी# मूल रूप से एक ऑब्जेक्ट ओरिएंटेड भाषा है लेकिन कुछ ऐसी स्थितियाँ होती हैं जहां आपको ट्रेडिशनल प्रोग्रामिंग परदे को तोड़ना पड़ेगा। इसीलिए, YAML के साथ काम करना आपके लिए मददगार हो सकता है।

## याम्ल कैसे करें

```C#
// YAML फाइल से डेटा पढ़ें
string yamlString = File.ReadAllText("data.yaml");

// YAML स्ट्रिंग से जावा स्ट्रिंग मैप बनाएं
var yamlObject = (YamlMappingNode)new YamlSerializer().Deserialize(new StringReader(yamlString));

// विशिष्ट की की डेटा को प्रिंट करें
Console.WriteLine(yamlObject.Children[new YamlScalarNode("key")].ToString());

// YAML मैप से ब्यूलियन वैल्यू निकालें
bool value = yamlObject.Children[new YamlScalarNode("boolKey")];

Console.WriteLine(value);
```

आउटपुट:
```
Value 1
True
```

## गहराई में जानें

YAML (YAML Ain't Markup Language) एक फ़ाइल प्रारूप है जो डेटा को हमारी आसानी से पढ़ने और लिखने की देता है। यह एक सिंपल और इंट्यूटिव फॉर्मैट है जिस्से पाठ परिभाषित हो सकते हैं तो इसे किसी दोस्त की तरह समझा जा सकता है। आप YAML को सी# में स्ट्रिंग या ऍरे की शक्ल में रीड कर सकते हैं और उसे .NET ऑब्जेक्टों में बदल सकते हैं।

## देखें भी

- [C# में YAML कैसे कोड करें](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-read-text-from-a-file)
- [YAML डोकुमेंटेशन](https://yaml.org/)
- [YamlDotNet लाइब्रेरी](https://github.com/aaubry/YamlDotNet)
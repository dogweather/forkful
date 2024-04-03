---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:12.933555-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: C# \u092E\u0947\u0902\
  \ YAML \u0915\u0947 \u0932\u093F\u090F \u0915\u094B\u0908 \u092C\u093F\u0932\u094D\
  \u091F-\u0907\u0928 \u0938\u092A\u094B\u0930\u094D\u091F \u0928\u0939\u0940\u0902\
  \ \u0939\u0948, \u0932\u0947\u0915\u093F\u0928 \u0906\u092A *YamlDotNet* \u091C\u0948\
  \u0938\u0947 \u0924\u0940\u0938\u0930\u0947 \u092A\u0915\u094D\u0937 \u0915\u0947\
  \ \u092A\u0941\u0938\u094D\u0924\u0915\u093E\u0932\u092F\u094B\u0902 \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 YAML \u0915\u0947 \u0938\
  \u093E\u0925 \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u0915\u093E\u092E \u0915\
  \u0930\u2026"
lastmod: '2024-03-13T22:44:52.367201-06:00'
model: gpt-4-0125-preview
summary: "C# \u092E\u0947\u0902 YAML \u0915\u0947 \u0932\u093F\u090F \u0915\u094B\u0908\
  \ \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u0938\u092A\u094B\u0930\u094D\u091F\
  \ \u0928\u0939\u0940\u0902 \u0939\u0948, \u0932\u0947\u0915\u093F\u0928 \u0906\u092A\
  \ *YamlDotNet* \u091C\u0948\u0938\u0947 \u0924\u0940\u0938\u0930\u0947 \u092A\u0915\
  \u094D\u0937 \u0915\u0947 \u092A\u0941\u0938\u094D\u0924\u0915\u093E\u0932\u092F\
  \u094B\u0902 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947\
  \ YAML \u0915\u0947 \u0938\u093E\u0925 \u0906\u0938\u093E\u0928\u0940 \u0938\u0947\
  \ \u0915\u093E\u092E \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964\
  \ \u0938\u092C\u0938\u0947 \u092A\u0939\u0932\u0947, \u0906\u092A\u0915\u094B YamlDotNet\
  \ \u092A\u0948\u0915\u0947\u091C \u0907\u0902\u0938\u094D\u091F\u0949\u0932 \u0915\
  \u0930\u0928\u093E \u0939\u094B\u0917\u093E."
title: "YAML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 41
---

## कैसे करें:
C# में YAML के लिए कोई बिल्ट-इन सपोर्ट नहीं है, लेकिन आप *YamlDotNet* जैसे तीसरे पक्ष के पुस्तकालयों का उपयोग करके YAML के साथ आसानी से काम कर सकते हैं। सबसे पहले, आपको YamlDotNet पैकेज इंस्टॉल करना होगा:

```bash
Install-Package YamlDotNet -Version 11.2.1
```

### YAML पढ़ना:
मान लीजिए आपके पास `config.yaml` नामक एक YAML फ़ाइल है जिसमें निम्नलिखित सामग्री है:
```yaml
appSettings:
  name: MyApp
  version: 1.0.0
```

आप इस YAML फाइल को C# में इस तरह पढ़ और पार्स कर सकते हैं:
```csharp
using System;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

public class AppConfig
{
    public AppSettings appSettings { get; set; }
}

public class AppSettings
{
    public string name { get; set; }
    public string version { get; set; }
}

class Program
{
    static void Main(string[] args)
    {
        var yaml = File.ReadAllText("config.yaml");
        var deserializer = new DeserializerBuilder()
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // समुचित नामकरण सम्मेलन के अनुसार समायोजित करें
            .Build();

        var config = deserializer.Deserialize<AppConfig>(yaml);

        Console.WriteLine($"Name: {config.appSettings.name}, Version: {config.appSettings.version}");
    }
}
```
**नमूना आउटपुट:**
```
Name: MyApp, Version: 1.0.0
```

### YAML लिखना:
YAML फ़ाइल में डेटा लिखने के लिए, YamlDotNet से `Serializer` क्लास का उपयोग करें। यहाँ एक ऑब्जेक्ट को वापस YAML में सीरियलाइज़ करने का तरीका है:

```csharp
using System;
using System.IO;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;

class Program
{
    static void Main(string[] args)
    {
        var config = new AppConfig
        {
            appSettings = new AppSettings
            {
                name = "MyApp",
                version = "2.0.0"
            }
        };

        var serializer = new SerializerBuilder()
            .WithNamingConvention(UnderscoredNamingConvention.Instance) // समुचित नामकरण सम्मेलन के अनुसार समायोजित करें
            .Build();

        var yaml = serializer.Serialize(config);
        File.WriteAllText("updatedConfig.yaml", yaml);

        Console.WriteLine(yaml);
    }
}
```
**नमूना आउटपुट:**
```yaml
appSettings:
  name: MyApp
  version: 2.0.0
```

यह सीधे तरीके से दर्शाता है कि आप अपने C# परियोजनाओं में YAML के साथ कुशलतापूर्वक कैसे काम कर सकते हैं, जिससे YamlDotNet पुस्तकालय का उपयोग करके YAML फ़ाइलों से पढ़ना और लिखना सरल हो जाता है।

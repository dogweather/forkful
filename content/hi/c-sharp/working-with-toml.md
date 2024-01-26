---
title:                "TOML के साथ काम करना"
date:                  2024-01-26T04:21:37.675889-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/working-with-toml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
TOML, टॉम की स्पष्ट, न्यूनतम भाषा के लिए एक संक्षिप्त नाम है, जो कि एक कॉन्फ़िगरेशन फ़ाइल प्रारूप है जिसे इसके स्पष्ट सेमांटिक्स के कारण पढ़ना आसान होता है। प्रोग्रामर इसे कॉन्फ़िगरेशन फ़ाइलों के लिए, सिस्टमों के बीच डेटा के आदान-प्रदान को सरल बनाने के लिए, और इसलिए उपयोग करते हैं क्योंकि यह मानव पठनीयता और मशीन पार्स़्यक्षमता के बीच एक संतुलन बनाता है।

## कैसे करें:
सबसे पहले, `Tomlyn` जैसे TOML पार्सर को स्थापित करें। अपने पैकेज मैनेजर का उपयोग करें:

```csharp
dotnet add package Tomlyn
```

अगला, एक TOML फ़ाइल को पार्स करें:

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"मालिक: {tomlTable["owner"]["name"]}");
// आउटपुट:
// मालिक: Tom Preston-Werner
```

अब, TOML बनाएं और लिखें:

```csharp
using Tomlyn;
using Tomlyn.Syntax;
using System;
using System.IO;

var doc = new DocumentSyntax
{
    Tables =
    {
        new TableSyntax("owner")
        {
            Items =
            {
                { "name", "Tom Preston-Werner" },
                { "dob", "1979-05-27T07:32:00Z" }
            }
        }
    }
};

var tomlString = doc.ToString();
File.WriteAllText("config.toml", tomlString);
Console.WriteLine("TOML config.toml में लिखा गया है");
// आउटपुट:
// TOML config.toml में लिखा गया है
```

## गहराई में जानकारी:
TOML को टॉम प्रेस्टन-वर्नर, गिटहब के सह-संस्थापक द्वारा, 2013 के आसपास, YAML और JSON जैसे मौजूदा प्रारूपों की कॉन्फ़िगरेशन सेटिंग्स में सीमाओं के प्रतिक्रिया स्वरूप बनाया गया था। यह विशेष रूप से कॉन्फ़िगरेशनों के लिए डिज़ाइन की गई है, जिसमें सीधेपन और एकसारता पर जोर दिया गया है।

वैकल्पिक कॉन्फ़िगरेशन प्रारूपों में YAML, JSON, और XML शामिल हैं। फिर भी, TOML विशेषकर कॉन्फ़िगरेशन फ़ाइलों के लिए अधिक मानव-अनुकूल होने के लिए उल्लेखनीय है, जहां हाथ से संपादन सामान्य है। JSON, जबकि सर्वव्यापी है, जटिल कॉन्फ़िगरेशनों के लिए कम पठनीय है, और XML शब्दाडंबरपूर्ण है। YAML, हालांकि पठनीयता में समान है, स्थान (whitespace) के भारी उपयोग के साथ जटिल हो सकता है और कुछ सामग्री के साथ सुरक्षा जोखिम है।

कार्यान्वयन के दृष्टिकोण से, TOML एक हैश टेबल के लिए साफ-सुथरा मानचित्रण पर केंद्रित है, जिससे डेटा निष्कर्षण प्रत्याशित होता है। संस्करण 1.0.0 जारी होने के साथ, TOML ने अपनी विशिष्टता को सुदृढ़ कर दिया, स्थिरता और टूलिंग समर्थन में सुधार किया।

## यह भी देखें:
- आधिकारिक TOML GitHub रेपो और विशिष्टता: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Tomlyn, .NET पुस्तकालय: [github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)
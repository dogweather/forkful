---
date: 2024-01-26 04:21:37.675889-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u0938\u092C\u0938\
  \u0947 \u092A\u0939\u0932\u0947, `Tomlyn` \u091C\u0948\u0938\u0947 TOML \u092A\u093E\
  \u0930\u094D\u0938\u0930 \u0915\u094B \u0938\u094D\u0925\u093E\u092A\u093F\u0924\
  \ \u0915\u0930\u0947\u0902\u0964 \u0905\u092A\u0928\u0947 \u092A\u0948\u0915\u0947\
  \u091C \u092E\u0948\u0928\u0947\u091C\u0930 \u0915\u093E \u0909\u092A\u092F\u094B\
  \u0917 \u0915\u0930\u0947\u0902."
lastmod: '2024-03-13T22:44:52.372359-06:00'
model: gpt-4-0125-preview
summary: "\u0938\u092C\u0938\u0947 \u092A\u0939\u0932\u0947, `Tomlyn` \u091C\u0948\
  \u0938\u0947 TOML \u092A\u093E\u0930\u094D\u0938\u0930 \u0915\u094B \u0938\u094D\
  \u0925\u093E\u092A\u093F\u0924 \u0915\u0930\u0947\u0902\u0964 \u0905\u092A\u0928\
  \u0947 \u092A\u0948\u0915\u0947\u091C \u092E\u0948\u0928\u0947\u091C\u0930 \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0947\u0902."
title: "TOML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 39
---

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

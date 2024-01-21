---
title:                "नई परियोजना शुरू करना"
date:                  2024-01-20T18:03:38.599679-07:00
model:                 gpt-4-1106-preview
simple_title:         "नई परियोजना शुरू करना"
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
नया प्रोजेक्ट शुरू करना C# के मौजूदा वर्जन में आपके आइडिया को कोड में बदलने की शुरुआत है। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि यह नए सॉफ्टवेयर बनाने, रिसर्च करने, या सीखने का पहला कदम होता है।

## How to (कैसे करें):
एक नया प्रोजेक्ट बनाने के लिए Visual Studio या .NET CLI का इस्तेमाल कर सकते हैं। यहाँ एक उदाहरण है:

```C#
// Visual Studio का उपयोग करके:
// File -> New -> Project मेनू ऑप्शन से नया प्रोजेक्ट बना सकते हैं।

// .NET CLI का उपयोग करके:
// PowerShell या Command Prompt खोलें और नीचे कमांड लिखें:
dotnet new console -o MyNewProject
cd MyNewProject
dotnet run
```

आपको कुछ इस तरह का आउटपुट दिखेगा:

```
Hello World!
```

## Deep Dive (गहरी जानकारी):
.NET फ्रेमवर्क और .NET Core के बाद, .NET 5 और उसके बाद के वर्जन ने सी# प्रोजेक्ट शुरू करने की प्रक्रिया को सरल बनाया है। अब CLI का इस्तेमाल करके बहुत आसानी से नया प्रोजेक्ट बनाया जा सकता है, जो कि Visual Studio के भारी इंटरफेस से बचने का एक विकल्प हो सकता है। सी# में आप Console Applications, Web Applications, Libraries व अन्य कई प्रकार के प्रोजेक्ट्स बना सकते हैं। `dotnet new` कमांड में कई सारे टेम्पलेट होते हैं जो विभिन्न प्रकार के प्रोजेक्ट्स के लिए नींव प्रदान करते हैं। 

## See Also (अधिक जानकारी के लिए):
- Microsoft Documentation: [.NET CLI overview](https://docs.microsoft.com/en-us/dotnet/core/tools/)
- Tutorial: [Create a new project in Visual Studio](https://docs.microsoft.com/en-us/visualstudio/ide/create-new-project?view=vs-2022)
- .NET Project Templates: [List of templates for the dotnet new command](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-new)
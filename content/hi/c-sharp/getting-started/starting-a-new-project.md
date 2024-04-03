---
date: 2024-01-20 18:03:38.599679-07:00
description: "How to (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902): \u090F\u0915\
  \ \u0928\u092F\u093E \u092A\u094D\u0930\u094B\u091C\u0947\u0915\u094D\u091F \u092C\
  \u0928\u093E\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F Visual Studio \u092F\u093E\
  \ .NET CLI \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\
  \ \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u092F\u0939\u093E\u0901 \u090F\
  \u0915 \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
lastmod: '2024-03-13T22:44:52.332697-06:00'
model: gpt-4-1106-preview
summary: "\u090F\u0915 \u0928\u092F\u093E \u092A\u094D\u0930\u094B\u091C\u0947\u0915\
  \u094D\u091F \u092C\u0928\u093E\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F Visual\
  \ Studio \u092F\u093E .NET CLI \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\
  \u093E\u0932 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u092F\
  \u0939\u093E\u0901 \u090F\u0915 \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948\
  ."
title: "\u0928\u0908 \u092A\u0930\u093F\u092F\u094B\u091C\u0928\u093E \u0936\u0941\
  \u0930\u0942 \u0915\u0930\u0928\u093E"
weight: 1
---

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

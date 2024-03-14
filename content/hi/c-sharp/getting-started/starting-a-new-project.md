---
date: 2024-01-20 18:03:38.599679-07:00
description: "\u0928\u092F\u093E \u092A\u094D\u0930\u094B\u091C\u0947\u0915\u094D\u091F\
  \ \u0936\u0941\u0930\u0942 \u0915\u0930\u0928\u093E C# \u0915\u0947 \u092E\u094C\
  \u091C\u0942\u0926\u093E \u0935\u0930\u094D\u091C\u0928 \u092E\u0947\u0902 \u0906\
  \u092A\u0915\u0947 \u0906\u0907\u0921\u093F\u092F\u093E \u0915\u094B \u0915\u094B\
  \u0921 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u0947 \u0915\u0940 \u0936\u0941\
  \u0930\u0941\u0906\u0924 \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0907\u0938\u0932\u093F\
  \u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0915\u094D\u092F\u094B\u0902\
  \u0915\u093F \u092F\u0939 \u0928\u090F \u0938\u0949\u092B\u094D\u091F\u0935\u0947\
  \u092F\u0930 \u092C\u0928\u093E\u0928\u0947,\u2026"
lastmod: '2024-03-13T22:44:52.332697-06:00'
model: gpt-4-1106-preview
summary: "\u0928\u092F\u093E \u092A\u094D\u0930\u094B\u091C\u0947\u0915\u094D\u091F\
  \ \u0936\u0941\u0930\u0942 \u0915\u0930\u0928\u093E C# \u0915\u0947 \u092E\u094C\
  \u091C\u0942\u0926\u093E \u0935\u0930\u094D\u091C\u0928 \u092E\u0947\u0902 \u0906\
  \u092A\u0915\u0947 \u0906\u0907\u0921\u093F\u092F\u093E \u0915\u094B \u0915\u094B\
  \u0921 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u0947 \u0915\u0940 \u0936\u0941\
  \u0930\u0941\u0906\u0924 \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0907\u0938\u0932\u093F\
  \u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0915\u094D\u092F\u094B\u0902\
  \u0915\u093F \u092F\u0939 \u0928\u090F \u0938\u0949\u092B\u094D\u091F\u0935\u0947\
  \u092F\u0930 \u092C\u0928\u093E\u0928\u0947,\u2026"
title: "\u0928\u0908 \u092A\u0930\u093F\u092F\u094B\u091C\u0928\u093E \u0936\u0941\
  \u0930\u0942 \u0915\u0930\u0928\u093E"
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

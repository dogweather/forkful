---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:26.746742-07:00
description: "C# \u092E\u0947\u0902 \u0928\u093F\u092F\u092E\u093F\u0924 \u0905\u092D\
  \u093F\u0935\u094D\u092F\u0915\u094D\u0924\u093F\u092F\u093E\u0901 (regex) \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u0947 \u092D\u0940\
  \u0924\u0930 \u092A\u0948\u091F\u0930\u094D\u0928 \u092E\u093F\u0932\u093E\u0928\
  \ \u0915\u0947 \u0932\u093F\u090F \u090F\u0915 \u0936\u0915\u094D\u0924\u093F\u0936\
  \u093E\u0932\u0940 \u0909\u092A\u0915\u0930\u0923 \u0939\u0948\u0902, \u091C\u094B\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094B\u0902 \u0915\
  \u094B \u0921\u0947\u091F\u093E \u0915\u094B \u0916\u094B\u091C\u0928\u0947, \u092C\
  \u0926\u0932\u0928\u0947, \u0935\u093F\u092D\u093E\u091C\u093F\u0924 \u0915\u0930\
  \u0928\u0947\u2026"
lastmod: '2024-02-25T18:49:49.514632-07:00'
model: gpt-4-0125-preview
summary: "C# \u092E\u0947\u0902 \u0928\u093F\u092F\u092E\u093F\u0924 \u0905\u092D\u093F\
  \u0935\u094D\u092F\u0915\u094D\u0924\u093F\u092F\u093E\u0901 (regex) \u0938\u094D\
  \u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u0947 \u092D\u0940\u0924\
  \u0930 \u092A\u0948\u091F\u0930\u094D\u0928 \u092E\u093F\u0932\u093E\u0928 \u0915\
  \u0947 \u0932\u093F\u090F \u090F\u0915 \u0936\u0915\u094D\u0924\u093F\u0936\u093E\
  \u0932\u0940 \u0909\u092A\u0915\u0930\u0923 \u0939\u0948\u0902, \u091C\u094B \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094B\u0902 \u0915\u094B\
  \ \u0921\u0947\u091F\u093E \u0915\u094B \u0916\u094B\u091C\u0928\u0947, \u092C\u0926\
  \u0932\u0928\u0947, \u0935\u093F\u092D\u093E\u091C\u093F\u0924 \u0915\u0930\u0928\
  \u0947\u2026"
title: "\u0930\u0947\u0917\u0941\u0932\u0930 \u090F\u0915\u094D\u0938\u092A\u094D\u0930\
  \u0947\u0936\u0928\u094D\u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\
  \u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
C# में नियमित अभिव्यक्तियाँ (regex) स्ट्रिंग्स के भीतर पैटर्न मिलान के लिए एक शक्तिशाली उपकरण हैं, जो प्रोग्रामरों को डेटा को खोजने, बदलने, विभाजित करने या निष्कर्षण करने में कुशलता से सक्षम बनाती हैं। प्रोग्रामर इसकी लचीलापन और प्रदर्शन के कारण सरल सत्यापनों से लेकर, जैसे कि ईमेल प्रारूप की जाँच करना, जटिल पाठ प्रसंस्करण कार्यों तक के लिए regex का उपयोग करते हैं।

## कैसे:

### साधारण पैटर्न मिलान
यदि एक स्ट्रिंग में एक विशेष पैटर्न होता है, तो आप `System.Text.RegularExpressions` नेमस्पेस से `Regex.IsMatch` मेथड का उपयोग कर सकते हैं।

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Hello, World!";
        string pattern = "World";
        bool containsPattern = Regex.IsMatch(sampleText, pattern);

        Console.WriteLine(containsPattern);  // आउटपुट: True
    }
}
```

### डेटा निष्कर्षण
रेगेक्स में समूहों का उपयोग करके एक स्ट्रिंग से डेटा निष्कर्ष किया जा सकता है जिसे `Regex.Match` मेथड के साथ किया जा सकता है।

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Date: 2023-04-12";
        string pattern = @"Date: (\d{4})-(\d{2})-(\d{2})";
        Match match = Regex.Match(sampleText, pattern);

        if (match.Success)
        {
            Console.WriteLine($"Year: {match.Groups[1].Value}");  // आउटपुट: वर्ष: 2023
            Console.WriteLine($"Month: {match.Groups[2].Value}");  // आउटपुट: माह: 04
            Console.WriteLine($"Day: {match.Groups[3].Value}");  // आउटपुट: दिवस: 12
        }
    }
}
```

### पाठ बदलना
`Regex.Replace` मेथड आपको एक स्ट्रिंग में एक निर्दिष्ट पैटर्न से मिलान करने वाले पाठ को बदलने देता है।

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Visit Microsoft!";
        string pattern = "Microsoft";
        string replacement = "Google";

        string result = Regex.Replace(sampleText, pattern, replacement);

        Console.WriteLine(result);  // आउटपुट: Visit Google!
    }
}
```

### स्ट्रिंग्स विभाजित करना
आप `Regex.Split` मेथड का उपयोग करके एक रेगेक्स पैटर्न के आधार पर एक स्ट्रिंग को एक सरणी में विभाजित कर सकते हैं।

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "one,two,three,four,five";
        string pattern = ",";

        string[] result = Regex.Split(sampleText, pattern);

        foreach (string item in result)
        {
            Console.WriteLine(item);
        }
        // आउटपुट: 
        // one
        // two
        // three
        // four
        // five
    }
}
```

### तृतीय-पक्ष पुस्तकालयों का उपयोग करना
जबकि .NET ढांचा नियमित अभिव्यक्तियों के लिए व्यापक समर्थन प्रदान करता है, `PCRE.NET` जैसे तृतीय-पक्ष पुस्तकालय भी हैं जो C# में Perl-संगत नियमित अभिव्यक्तियाँ (PCRE) प्रदान करते हैं। यह उपयोगी हो सकता है यदि आपको .NET के कार्यान्वयन में उपलब्ध नहीं विशेषताओं या सिन्टैक्स की आवश्यकता हो।

`PCRE.NET` का उपयोग करने के लिए, आप पहले इसके NuGet पैकेज को स्थापित करेंगे, और फिर आप इसे मूल .NET regex क्लासों का उपयोग करने के समान तरीक़े से उपयोग कर सकते हैं।

```csharp
// PCRE.NET का उपयोग करने का उदाहरण यहाँ है
// ध्यान दें: PCRE.NET की एक विशेषता को प्रदर्शित करने के लिए ऊपर दिए गए नमूनों के समान एक नमूना कल्पना करें।
```

नियमित अभिव्यक्तियों के लिए तृतीय-पक्ष पुस्तकालयों का एकीकरण करते समय, हमेशा विस्तृत उपयोग और संगतता की जानकारी के लिए उनके दस्तावेज़ों का परामर्श लें।

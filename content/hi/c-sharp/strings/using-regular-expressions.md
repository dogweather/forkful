---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:26.746742-07:00
description: "\u0915\u0948\u0938\u0947: \u092F\u0926\u093F \u090F\u0915 \u0938\u094D\
  \u091F\u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u090F\u0915 \u0935\u093F\
  \u0936\u0947\u0937 \u092A\u0948\u091F\u0930\u094D\u0928 \u0939\u094B\u0924\u093E\
  \ \u0939\u0948, \u0924\u094B \u0906\u092A `System.Text.RegularExpressions` \u0928\
  \u0947\u092E\u0938\u094D\u092A\u0947\u0938 \u0938\u0947 `Regex.IsMatch` \u092E\u0947\
  \u0925\u0921 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930 \u0938\u0915\
  \u0924\u0947 \u0939\u0948\u0902\u0964."
lastmod: '2024-03-13T22:44:52.314357-06:00'
model: gpt-4-0125-preview
summary: "\u092F\u0926\u093F \u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\
  \u0917 \u092E\u0947\u0902 \u090F\u0915 \u0935\u093F\u0936\u0947\u0937 \u092A\u0948\
  \u091F\u0930\u094D\u0928 \u0939\u094B\u0924\u093E \u0939\u0948, \u0924\u094B \u0906\
  \u092A `System.Text.RegularExpressions` \u0928\u0947\u092E\u0938\u094D\u092A\u0947\
  \u0938 \u0938\u0947 `Regex.IsMatch` \u092E\u0947\u0925\u0921 \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\
  \u0964."
title: "\u0930\u0947\u0917\u0941\u0932\u0930 \u090F\u0915\u094D\u0938\u092A\u094D\u0930\
  \u0947\u0936\u0928\u094D\u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\
  \u0930\u0928\u093E"
weight: 11
---

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

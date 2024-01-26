---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:48:45.184716-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Random numbers उत्पन्न करना यानी ऐसे नंबर्स बनाना जिसका कोई पैटर्न न हो। Programmers इसे games, simulations, security systems, aur data analysis में use करते हैं।

## How to: (कैसे करें:)
```C#
using System;

class RandomNumberExample
{
    static void Main()
    {
        // Random number generator instance
        Random rand = new Random();
        
        // Generate a random number between 0 and 100
        int randomNumber = rand.Next(0, 100);
        Console.WriteLine($"Generated Random Number: {randomNumber}");
    }
}
```
Sample Output ऐसा होगा:
```
Generated Random Number: 47
```

## Deep Dive (गहराई से जानकारी)
C# में `System.Random` class से 1990's में ही random number generators ka use शुरू हुआ। यह Pseudorandom numbers generate करता है जिसका मतलब है कि वे असल में random नहीं होते अगर आपको seed pata हो तो। True randomness के लिए, `System.Security.Cryptography` namespace का use करें जैसे `RNGCryptoServiceProvider` class, पर यह ज्यादा संसाधन खपत कर सकता है। 

Randomness की quality improve करने के लिए हर बार एक नया seed value देना बेहतर होता है, जैसे कि system time। याद रखें, `Random` class thread-safe नहीं है। अगर आपको एक से ज्यादा threads में random numbers चाहिए, तो lock का use करें या `ThreadLocal<Random>` instances का प्रयोग करें।

## See Also (और जानकारी के लिए)
- Microsoft Docs on `Random` Class: [Microsoft Docs Random](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netframework-4.8)
- Random Number Generation in Cryptography: [RNGCryptoServiceProvider Class](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=netframework-4.8)

---
title:                "सहयोगी अरेज़ का उपयोग करना"
aliases: - /hi/c-sharp/using-associative-arrays.md
date:                  2024-01-30T19:11:08.806001-07:00
model:                 gpt-4-0125-preview
simple_title:         "सहयोगी अरेज़ का उपयोग करना"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

सी# में एसोसिएटिव ऐरेज़, या शब्दकोश का प्रयोग करके आप कुंजी और मान के जोड़ों को संग्रहित और प्रबंधित कर सकते हैं। जब आपको एक अद्वितीय पहचानकर्ता के आधार पर तेज़ी से मूल्यों को प्राप्त करने की आवश्यकता होती है, तो वे आपकी पहली पसंद बन जाते हैं, जिससे जटिल अनुप्रयोगों में डेटा प्रबंधन एक हवा बन जाता है।

## कैसे करें:

सी# में, आप `Dictionary<TKey, TValue>` कक्षा का उपयोग करके एसोसिएटिव ऐरेज़ के साथ काम करते हैं। यहाँ शुरू करने के लिए एक त्वरित उदाहरण है:

```C#
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        // एक शब्दकोश बना रहा हूँ
        Dictionary<string, int> fruitBasket = new Dictionary<string, int>();

        // कुंजी-मान जोड़े जोड़ना
        fruitBasket.Add("Apples", 5);
        fruitBasket.Add("Oranges", 10);

        // इसकी कुंजी का उपयोग करके एक मान तक पहुँचना
        Console.WriteLine("Apples: " + fruitBasket["Apples"]);
        
        // एक मान को अद्यतन करना
        fruitBasket["Apples"] = 7;
        Console.WriteLine("Updated Apples: " + fruitBasket["Apples"]);
        
        // एक कुंजी-मान जोड़ी को हटाना
        fruitBasket.Remove("Oranges");

        // शब्दकोश के ऊपर इटरेट करना
        foreach (var pair in fruitBasket)
        {
            Console.WriteLine(pair.Key + ": " + pair.Value);
        }
    }
}
```
नमूना आउटपुट:
```
Apples: 5
Updated Apples: 7
Apples: 7
```

यह उदाहरण एक शब्दकोश बनाने, जोड़ने, पहुँचाने, अद्यतन करने और हटाने, और इसके ऊपर इटरेट करने को प्रदर्शित करता है।

## गहराई में जाना

एसोसिएटिव ऐरेज़ की धारणा Perl और PHP जैसी स्क्रिप्टिंग भाषाओं में उनके उपयोग से वापस आती है, जहाँ वे डेटा के संग्रह का प्रबंधन करने में लचीलापन प्रदान करते हैं। सी# में, `Dictionary<TKey, TValue>` डी फैक्टो कार्यान्वयन है, जिसे .NET Framework 2.0 में पेश किया गया था। यह डेटा को हैश टेबल में संग्रहित करता है, जिससे त्वरित लुक-अप, जोड़ और हटाने सुनिश्चित होते हैं।

हालांकि, यह ध्यान देने योग्य है कि जबकि शब्दकोश अत्यंत बहुमुखी होते हैं, वे हमेशा आपके सर्वश्रेष्ठ दाँव नहीं हो सकते हैं। आदेशित संग्रहों को बनाए रखने के लिए, आप `SortedDictionary<TKey, TValue>` या `SortedList<TKey, TValue>` को देख सकते हैं, जो धीमी डालने और निकालने की क्रियाओं की कीमत पर सॉर्ट किए गए क्रम प्रदान करते हैं। धागा-सुरक्षा की मांग करने वाली परिस्थितियों के लिए, `ConcurrentDictionary<TKey, TValue>` अतिरिक्त लागत जोड़ता है लेकिन मैन्युअल लॉकिंग के बिना बहु-धागा से सुरक्षित पहुँच सुनिश्चित करता है।

अंततः, सी# में एक एसोसिएटिव ऐरेज़ कार्यान्वयन की पसंद आपकी विशिष्ट आवश्यकताओं पर निर्भर करती है जैसे कि क्रम, प्रदर्शन, और धागा सुरक्षा।

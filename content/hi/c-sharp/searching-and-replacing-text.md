---
title:                "C#: टेक्स्ट खोज और प्रतिस्थापन करना"
simple_title:         "टेक्स्ट खोज और प्रतिस्थापन करना"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी टेक्स्ट के साथ काम करने में समय लगता है, और जब आपको सेट ऑफ़ टेक्स्टों को बदलना हो, तो यह उन्हें गणना और बदलने के लिए एक कठिन और दर्दनाक काम हो सकता है। इस समस्या को हल करने के लिए, हमें text searching और replacing की आवश्यकता होती है। इस लेख में, हम देखेंगे कि आप C# में कैसे एक सरल मेथड का उपयोग करके जटिल टेक्स्ट को आसानी से बदल सकते हैं। 

## कैसे करें

सरल स्थान पर हस्तक्षेप कैसे करे: 
```C#
string text = "यह एक टेक्स्ट है।";
string newText = text.Replace("है", "होना चाहिए");
Console.WriteLine(newText);
```
आउटपुट: यह एक टेक्स्ट होना चाहिए।

कैसे ऑनलाइन करे गणना और स्थान बदले: 
```C#
string text = "यह भंगुर है। यह एक बेहतरीन टेक्स्ट है। यह भंगुर है।";
string[] bhangurWords = text.Split('भंगुर');
text = string.Join("शानदार", bhangurWords);
Console.WriteLine(text);
```
आउटपुट: यह शानदार है। यह एक बेहतरीन टेक्स्ट है। यह शानदार है।

## गहराई से जाएं

C# में text searching और replacing को आसान बनाने के लिए, हमें String.Replace और String.Split जैसे प्रयोग विधियों का उपयोग करना होता है। String.Replace भए की स्थानिक बदलाव के लिए उपयोग किया जाता है, जबकि String.Split भंगुर और शानदार जैसे पाठ तालिका को विभाजित करने और पुनर्स्थापन करने के लिए उपयोग किया जाता है। हमेशा ध्यान रखें कि टेक्स्ट के साथ काम करने में Regex और अन्य सहायक उपकरणों का भी उपय
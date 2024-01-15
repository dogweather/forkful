---
title:                "टेक्स्ट को खोजना और बदलना"
html_title:           "C#: टेक्स्ट को खोजना और बदलना"
simple_title:         "टेक्स्ट को खोजना और बदलना"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्यों

यहां हम बात करेंगे कि किस प्रकार हम टेक्स्ट को खोज और प्रतिस्थापित कर सकते हैं। टेक्स्ट को खोज और प्रतिस्थापित करने को एक अग्रिम प्रोग्रामर और कोडर की आवश्यकता होती है। यह भागीदारी कोडिंग प्रक्रिया को बेहतर बनाता है और कोड रीफैक्टरिंग को आसान बनाता है।

## कैसे

हम टेक्स्ट को खोजने और प्रतिस्थापित करने के लिए 2 तरीके बारे में बात करेंगे। पहला तरीका है `Replace()` फ़ंक्शन का उपयोग करके। यह फ़ंक्शन दोस्ताना साधन है जिससे हम टेक्स्ट को खोज और प्रतिस्थापित कर सकते हैं। दूसरा तरीका है `Regex` कक्ष का उपयोग करना, जो एक शक्तिशाली टेक्स्ट खोज और प्रतिस्थापन इंजन है। नीचे दिए गए उदाहरण में हम दोनों तरीकों का प्रदर्शन देखेंगे।

```
C# string str = "Hello, world."; 
string newStr = str.Replace("world", "C# programming"); 
Console.WriteLine(newStr); 
```

उत्पाद के रूप में, यह प्रोग्राम "Hello, C# programming." छापेगा।

```
C# using System.Text.RegularExpressions;
string str = "Today is a beautiful day.";
string pattern = "a";
string newStr = Regex.Replace(str, pattern, "an amazing", RegexOptions.IgnoreCase);
Console.WriteLine(newStr);
```

उत्पाद के रूप में, यह प्रोग्राम "Todan amazingy is an amazing beautan amazingiful dyan amazing." छापेगा।

## गहराई में जाने

टेक्स्ट को खोजने और प्रतिस्थापित करने के लिए उपरोक्त उदाहरणों में, हमने सामान्य टेक्स्ट को बदलने के लिए वास्तविक उपकरणों का उपयोग किया है। लेकिन यदि आप अधिक सम्पर्क करना चाहते हैं, तो `Regex` कक्ष के साथ आप
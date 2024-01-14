---
title:                "C#: स्ट्रिंग को लोअर केस में रूपांतरण करना"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

अक्सर हम प्रोग्रामिंग में देखते हैं कि रखी हुई स्ट्रिंग हरूफ अक्षरों में होती है लेकिन जिस तरह की स्ट्रिंग हमारे प्रोग्राम को आसान बनाती है, वह है स्ट्रिंग को लोअर केस में बदलना। इससे हमारे कोड को पढ़ने में भी आसानी होती है और स्ट्रिंगों को साथ में तुलना भी करने में।

## कैसे करें

```C#
string str = "HELLO WORLD";
string lower = str.ToLower(); // lower = "hello world"

string num = "12345";
string lower = num.ToLower(); // lower = "12345" 
// क्योंकि समझ जाने के लिए यह पूरी तरह से काम करता है इसलिए यह एक अच्छा उदाहरण है।

string name = "John Doe";
for(int i = 0; i < name.Length; i++){
    Console.Write(name[i].ToString().ToLower());   
} // output: john doe
// यह भी हमें ये दिखाता है कि कैसे हम स्ट्रिंग के हर अक्षर को अलग अलग लोअर केस में प्रिंट कर सकते हैं।
```

## गहराई में जाएं

स्ट्रिंग्स को लोअर केस में बदलने के लिए, हम C# में इस्तेमाल किए गए ToLower() फंक्शन की मदद से स्ट्रिंग को लोअर केस में कनवर्ट कर सकते हैं। यह फंक्शन CultureInfo क्लास को पैरामीटर के रूप में आपने सशल ना देते हुए कोड में पूरी तरह से काम करता है। अगर आप स्विच केस का उपयोग करना चाहते हैं तो यह भी एक अच्छा विकल्प है। इसके अलावा, आप इस फंक्शन के साथ रिजर्व कीवर्ड using System.Globalization; का भी इस्तेमाल कर सकते हैं।

## देखें भी

स्ट्रिंग को जोड़ना: https://docs.microsoft.com/en-us/dotnet/api/system.string.concat

जिसमें भी आप ये देख सकते हैं कि कैसे आप स्ट्रिंग्स को जोड
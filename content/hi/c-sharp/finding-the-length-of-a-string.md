---
title:                "C#: स्ट्रिंग की लंबाई का पता लगाना"
simple_title:         "स्ट्रिंग की लंबाई का पता लगाना"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## आखिर क्यों?

आशा है कि आप सभी अच्छे होंगे! आज हम बात करेंगे कि हिंदी भाषा में केवल एक श्याम कैसे लंबाई स्ट्रिंग को निकाला जाता है। इस विषय में आपको उस असली कारण के बारे में बताया जाएगा कि लोग अंग्रेज़ी या हिंदी के मध्य अंतर को निकालना चाहते हैं। तो चलिए शुरू होते हैं!

## कैसे करें?
 
```C#
 string language = "हिंदी";
int length = language.Length; //length = 5
Console.WriteLine(length); //output: 5
```

अपनी स्ट्रिंग को जैसे ही आप लिखते हैं, उसकी लंबाई सॉफ्टवेयर के द्वारा स्वचालित रूप से कैल्कुलेट की जाती है और आपको उस लंबाई को निकालने की आवश्यकता नहीं होती है। हालांकि, अगर आपको अधिक कंट्रोल चाहिए या अन्य क्षेत्रों में अपनी प्रोग्रामिंग जानकारी में खोज करना चाहते हैं तो निम्नलिखित मेथड को देखें:

### .Length का उपयोग

```C#
string greeting = "नमस्ते!";
Console.WriteLine(greeting.Length); //output: 7

string name = "लता जी";
Console.WriteLine(name.Length); //output: 6
```

इस मेथड की मदद से आप अपनी स्ट्रिंग के लिए सही लंबाई जान सकते हैं। हैन आप अंग्रेज़ी या हिंदा स्ट्रिंग का इस्तेमाल क्यों भी करना चाहते हैं, आपको बस `string` और `.Length` का उपयोग करना होगा।

### .Length से अधिक कंट्रोल

```C#
string fullName = "आलोक वर्मा";
Console.WriteLine("पहला नाम: " + fullName.Substring(0, 4)); //output: "पहला नाम: आलोक"

string sentence = "हिंदी चलाते हैं";
Console.WriteLine("पहला शब्द: " + sentence.Split(" ")[0]); //output: "पहला शब्द: हिंदी"
```

अधिक कंट
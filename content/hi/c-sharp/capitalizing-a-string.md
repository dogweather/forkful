---
title:    "C#: एक स्ट्रिंग को मजबूत बनाना"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों
कोई भी व्यक्ति अपने स्ट्रिंग को capitalizing करने में आकर्षित क्यों होगा? यह काम अपने स्ट्रिंग को सबसे पहले छोटे अक्षरों से बड़े अक्षरों में परिवर्तित करने का एक आसान तरीका है! यह अपने कोड को और स्पष्ट और सुनिश्चित बनाने में मदद कर सकता है।

## कैसे करे
आप C# में अपने स्ट्रिंग को capitalizing करने के लिए बहुत सारे तरीके हो सकते हैं। एक मेथड ठीक तरीके से दिखाने के लिए हम तीन अलग-अलग method यूज़ करेंगे।

```C#
string str = "hello world"; // स्ट्रिंग बनाएं
string capitalizedStr = str.ToUpper(); //पूर्ण स्ट्रिंग को capitalizedStr में बदलें

Console.WriteLine(capitalizedStr); // आपको "HELLO WORLD" मिलेगा

string firstLetter = str.Substring(0, 1).ToUpper(); // पहले अक्षर को capitalized एक string में बदलें
string restOfStr = str.Substring(1); // बाकी सभी अक्षरों को अलग string में स्टोर करें

Console.WriteLine(firstLetter + restOfStr); // "Hello world" मिलेगा
```

## गहराई में जाएं
स्ट्रिंग capitalize करना बहुत अधिक समय और memory का खपाई कर सकता है, इसलिए बेहतर होगा कि आप उसमें अपनी स्ट्रिंग को modify कर दें। आप अपनी स्ट्रिंग को char array में convert कर सकते हैं और उसे modify करके दोबारा स्ट्रिंग में convert कर सकते हैं। यह आपके कोड के लिए अधिक भरोसेमंद और उचित हो सकता है।

## देखिए भी
"देखिए भी" (See Also):
- [C# char data type](https://www.tutorialspoint.com/csharp/csharp_char.htm)
- [String manipulation in C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/#manipulating-strings)
- [Difference between ToUpper and ToUpperInvariant in C#](https://www.codeproject.com/Tips/136811/To-Upper-vs-To-UpperInvariant-are-we-always-suppos)
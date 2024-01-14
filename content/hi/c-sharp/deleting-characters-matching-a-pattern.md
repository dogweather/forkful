---
title:    "C#: पैटर्न से मेल खाते अक्षरों को हटाना"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

"क्यों: आपको एक निश्चित पैटर्न से मेल खानेवाले अक्षरों को हटाने में रुचि हो सकती है।"

आपके कोड में अक्षरों के साथ साथ सदियों से भी उपयोग किया जा सकता है। यह एक उदाहरण है:

```C# 
string text = "यह वाकय जो कि एक पाठक से सम्बंधित है समर्पित है।"; 
string pattern = "एक"; 
Console.WriteLine("प्रारंभिक पाठ: " + text); 
string newText = text.Replace(pattern, ""); 
Console.WriteLine("नया पाठ: " + newText); 
```
 
बाहरवींज़र कोड का नतीजा प्रिंट होगा: 

प्रारंभिक पाठ: यह वाकय जो कि एक पाठक से सम्बंधित है समर्पित है। 

नया पाठ: यह वाकय जो कि पाठक से सम्बंधित है समर्पित है। 

गहराई में जाने के लिए: आप आखिरी चरण में हटाने के लिए पटर्न से मेल खाने वाले अक्षरों के बारे में अधिक जानकारी दे सकते हैं। यह में है कि प्रोग्रामिंग में इस तरह के रूप यूटिलिटियों की अनिवार्य गुण हैं।

"See Also:"

- https://www.geeksforgeeks.org/c-sharp-string-output/
- https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated
- https://www.w3schools.in/csharp-tutorial/basic-syntax/
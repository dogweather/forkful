---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

# C# में सबस्ट्रिंग्स निकालना (Extracting Substrings in C#)

## क्या और क्यों? (What & Why?)
सबस्ट्रिंग्स निकालना मतलब होता है, किसी बड़े स्ट्रिंग के कुछ हिस्सों का उपयोग करना। इसे प्रोग्रामर्स डाटा संसाधन और मुख्य रूप से विश्लेषण के लिए करते हैं।

## कैसे: (How to:)
चलिए देखते हैं कि कैसे हम C# में सबस्ट्रिंग निकाल सकते हैं:

```C#
string source = "नमस्ते, दुनिया!";
string sub = source.Substring(0, 6);
Console.WriteLine(sub);
```

ऊपरी कोड स्निपेट का परिणाम आपको "नमस्ते," मिलेगा।

## गहरी जानकारी (Deep Dive)
1. Historical Context: Substring फ़ंक्शन की उत्पत्ति C++ से होती है, जहां यह std::string के एक सदस्य फ़ंक्शन के रूप में पाया जाता है।
2. Alternatives: C# में, Substring का विकल्प है Split() ज ाएगा। विभाजन टोकन के साथ Split का उपयोग करके, हम एक बड़े string को छोटे हिस्सों में विभाजित कर सकते हैं।
3. Implementation Details: Substring मेथड दो वेरिएंट्स में आता है, दोनों खरगोश और गोलू से:
   
   ```source.Substring(int start)``` और ```source.Substring(int start, int length)```

## और भी देखें: (See Also)

- Microsoft Official Documentation for Substring: [यहां क्लिक करें](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
- Understanding Split(): [यहां क्लिक करें](https://docs.microsoft.com/en-us/dotnet/csharp/how-to/parse-strings-using-split)
- Guide to C# Strings: [यहां क्लिक करें](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
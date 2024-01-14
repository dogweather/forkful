---
title:    "C#: एक पैटर्न से मेल खाते अक्षरों को हटाना"
keywords: ["C#"]
---

{{< edit_this_page >}}

## क्यों

एक पैटर्न से मेल खाने वाले चरित्रों को हटाने में ईंतना लोग क्यों जुटे हुए होते हैं? यह कारण को अधिक सुविधाजनक बनाने के लिए, आइये हमारा यह लेख हिंदी में पढ़ें!

## कैसे करें

कुछ स्ट्रिंग मेथड्स इस तरह होते हैं जिसमें हम पैटर्न के अनुसार चरित्रों को हटा सकते हैं। नीचे दिए गए उदाहरणों में हम पैटर्न "a" के साथ स्थित चरित्रों को हटाने के लिए `Remove()` और `Replace()` मेथड का उपयोग करेंगे।

```C#
string str = "abcdeabcdef";
Console.WriteLine("Original string: " + str);
// पैटर्न "a" बाद के सभी चरित्रों को हटाएं
str = str.Remove(str.IndexOf("a") + 1);
Console.WriteLine("Using Remove(): " + str);
// पैटर्न "a" को "x" से बदलें
str = str.Replace("a", "x");
Console.WriteLine("Using Replace(): " + str);
```

आउटपुट:

```
Original string: abcdeabcdef
Using Remove(): abcdef
Using Replace(): xbcdef

```

## गहराई में जाएं

`Remove()` और `Replace()` दोनों ही `string` क्लास के सदस्य मेथड हैं जिनका उपयोग करके हम चरित्रों को हटा सकते हैं। `Remove()` मेथड में हम चरित्र का केवल एक आधार इंडेक्स देते हैं, जबकि `Replace()` मेथड में हम उस चरित्र को दोनों आधार इंडेक्स के बीच के चरित्रों से बदल सकते हैं। इसके अलावा, हम `Regex` (Regular Expression) भी उपयोग कर सकते हैं जो सभी प्रकार के पैटर्न के साथ काम करता है।

## इससे जुड़े लेख

[Microsoft Docs: `Remove()` मेथड](https://docs.microsoft.com/en-us/dotnet/api/system.string.remove?view=netframework-4.8) <br/>
[Microsoft Docs: `Replace()` मेथड](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netframework-4.8) <br/>
[Tutorialspoint: रेगुलर एक्सप्रेशन की परिचय](https://www.tutorialspoint.com/regex/regex_introduction.htm)
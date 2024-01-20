---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
विकल्पों का आविष्कार करने और पाठ को बदलने का कार्य सॉफ़्टवेयर डेवलपर्स किसी भी पाठ संसाधन को खोजने और उसे अन्य पाठ संसाधन द्वारा बदलने का प्रवेश द्वार है। यह सामान्य रूप से अवधारणाओं की लाइन में कटौती करने और संगणना को बनाए रखने के लिए किया जाता है। 

## कैसे करें:
C# में यह काम करने के लिए कई तरीके हैं, लेकिन यहां हम `String.Replace` का उपयोग करेंगे:

```C#
string originalText = "नमस्ते दुनिया";
string searchText = "दुनिया";
string replaceText = "भारत";

string newText = originalText.Replace(searchText, replaceText);
Console.WriteLine(newText); //Outputs "नमस्ते भारत"
```
यहां हमने वाक्य "नमस्ते दुनिया" में शब्द "दुनिया" को "भारत" से बदल दिया।

## गहरी डाइव
**ऐतिहासिक संदर्भ:** खोजने और बदलने की संकल्पना प्राचीन प्रोग्रामन के दिनों से ही अस्तित्व में है।
**विकल्प:** इसे ज्यादा आरोपयुक्त कस्टम क्लासेज, जैसे कि `Regex` द्वारा भी लागू किया गया है।
**व्याख्यान विवरण:** `String.Replace` विधि, खोजे गए पाठ की प्रत्येक उदाहरण को बदलने का काम करती है। यदि आप केवल प्रथम उदाहरण को बदलना चाहते हैं, तो आपको एक अलग तरीका अपनाना होगा।

## ऐसी भी देखें
1. `[Microsoft Docs: String.Replace Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-5.0)`
2. `[Microsoft Docs: Regex Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=net-5.0)`
3. `[StackOverflow: How to replace only the first match of a string in C#?](https://stackoverflow.com/questions/141045/how-to-replace-only-the-first-match-of-a-string-in-c-sharp)`
---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
नये भाषाभाषी डेवलपर्स के लिए, ध्यान दीजिए कि टेक्स्ट खोजना और बदलना मतलब है एक विशिष्ट टेक्स्ट खोजना और उसे दूसरे टेक्स्ट से बदलना। प्रोग्रामर्स इसे उन समस्याओं को हल करने में करते हैं जहां उन्हें एक विशेष स्ट्रिंग को एक डॉक्यूमेंट में अन्य स्ट्रिंग से बदलना हो।  

## कैसे करें:
चलिए कोडिंग के माध्यम से खोजने और बदलने की प्रक्रिया को समझते हैं।  

```Swift
var str = "नमस्ते, दुनिया!"
str = str.replacingOccurrences(of: "नमस्ते", with: "अलविदा")
print(str)
```
संकलित आउटपुट:

```Swift
"अलविदा, दुनिया!"
```
यहां हमने "नमस्ते" को "अलविदा" से बदल दिया।

## गहराई में:
Swift में 'replacingOccurrences' method हमें टेक्स्ट में खोजने और बदलने में मदद करता है। यह Foundation फ्रेमवर्क का हिस्सा है जिसने कोकोआ और कोकोआ टच साथ दिया। Swift में आप इसे 4.0 संस्करण से उपयोग कर सकते हैं।
  
## अन्य विकल्पों के बारे में:
Swift में लगभग सभी टेक्स्ट (String) operations के लिए बहुत सारे विधियों (methods) हैं। जैसे 'contains', 'split', 'starts(with:)' और 'ends(with:)' आदि। ये विधियाँ टेक्स्ट संरचना के उत्कृष्ट प्रबंधन की आवश्यकताओं को समझते हुए बनाई गईं हैं। 

## अन्य संसाधन:
Swift में विशेष टेक्स्ट ऑपरेशन्स के बारे में और गहराई से समझने के लिए, निम्नलिखित लिंक पर क्लिक करें -

- [Working With Strings in Swift](https://developer.apple.com/documentation/swift/string)
- [How To Replace Occurrences in a Swift String](https://www.hackingwithswift.com/example-code/strings/how-to-replace-occurrences-of-a-string-within-a-string)
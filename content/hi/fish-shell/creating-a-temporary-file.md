---
title:    "Fish Shell: अस्थायी फ़ाइल बनाना"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों

अगर आप फिश शेल प्रोग्रामिंग में नए हैं तो आपने शायद "temporary file" के बारे में सुना होगा। यह एक साधारण फाइल है जो कंप्यूटर में अस्थायी रूप से बनाई जाती है। लेकिन क्या आप जानते हैं कि आप कैसे फिश शेल में ताजगी और प्रोग्रामिंग कौशल बढ़ाने के लिए temporary files का उपयोग कर सकते हैं? इस ब्लॉग पोस्ट में, हम temporary file बनाना सीखेंगे और जानेंगे कि इसके लाभ क्या है।

## कैसे करें

अपने फिश शेल स्क्रिप्ट में temporary file बनाने के लिए हम निम्न चरणों का पालन करेंगे।

```
Fish Shell में Temporary File बनाते हुए:
touch (create empty file) command का उपयोग करके आप एक रिक्त temporary file बना सकते हैं।
नीचे दिए गए उदाहरण में, हमने Gradle build tool का उपयोग करके temporary file बनाया है।

```
```
# Temporary file बनाएं
touch temp.txt

# Temporary file में डेटा लिखें
echo "यह temporary file है" > temp.txt

# Temporary file का डेटा पढ़ें
cat temp.txt

# Temporary file डिलीट करें
rm temp.txt

इस सरल कोड के लिए हमने अपनी current डिरेक्टरी में temp.txt नामक एक temporary file बनाई और उसमें डेटा लिखा और पढ़ा। अंत में, हमने उसे डिलीट भी कर दिया।

## गहराई में

Temporary files के उपयोग के कई अन्य फायदे हैं। इन फायदों में से कुछ निम्नलिखित हैं:

- प्रोग्राम का दोहराव: Temporary file का उपयोग करके, आप अपनी प्रोग्राम के दोहराव को जाँच सकते हैं। आप temporary file में डाटा लिखकर, उसे पढ़कर और विशेष रूप से संपादित करके अपने प्रोग्राम का दोहर
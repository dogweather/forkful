---
title:    "Ruby: स्ट्रिंग्स को अभिक्षरण करना"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## क्यों

इस लेख में हम Ruby में स्ट्रिंग्स को एक साथ जोड़ने के बारे में जानेंगे। स्ट्रिंग्स को जोड़ना एक आम समस्या है जो कि हर प्रोग्रामर को अक्सर समान तरीके से प्रोग्रामिंग में होती है। इस लेख के माध्यम से हम स्ट्रिंग्स को जोड़ने के लिए आसान और असरदार तरीके से सीखेंगे।

## कैसे करें

में यहां पर हम स्ट्रिंग्स को जोड़ने का प्रोग्राम दिखाएंगे और इसका प्रतिक्रियाओं को भी देखेंगे। सबसे पहले हम एक स्ट्रिंग से आरंभ करेंगे और उसको एक दूसरी स्ट्रिंग से जोड़ेंगे। यह नीचे दिए गए मामले में दिखाया गया है:

```Ruby
str1 = "मैं"
str2 = "रूबी सीख रहा हूँ।"
puts str1 + str2 # Output: मैं रूबी सीख रहा हूँ।
```

इसमें हमने `+ ` ऑपरेटर का प्रयोग किया है जो दो स्ट्रिंग्स को आपस में जोड़ता है। आप देख सकते हैं कि जब दो स्ट्रिंग्स को जोड़ा जाता है तो वे एक साथ जुड़ जाते हैं और एक ही लाइन पर दिखाए जाते हैं।

यदि हमें दो स्ट्रिंग्स को सीधे जोड़ना हो, तो हम `concat` फ़ंक्शन का उपयोग कर सकते हैं। नीचे दिए गए मामले में यह भी दिखाया गया है:

```Ruby
str1 = "मेरा"
str2 = "भाषा"
str3 = "हिंदी है।"
str1.concat(str2, str3)
puts str1 # Output: मेरा भाषा हिंदी है।
```

इसमें हमने `concat` फ़ंक्शन को तीन स्ट्रिंग्स के साथ बुलाया और उसको परिणाम में
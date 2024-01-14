---
title:    "Haskell: टेक्स्ट को खोजें और बदलें"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## क्यों

टेक्स्ट को खोज और बदलने में लोगों को क्यों रुचि है, इसकी चर्चा यहां नहीं की जाएगी। लेकिन, सामान्य फ़ायदों को देखते हुए, यह पाठ का बदलाव करने, समान शब्दों को तलाशने और पूर्वानुमान बनाने में मदद कर सकता है।

## कैसे करें

यदि आप टेक्स्ट स्ट्रिंग में कुछ खोज और परिवर्तन करना चाहते हैं, तो हैस्केल में आसानी से कर सकते हैं। नीचे दिए गए कोड ब्लॉक में, आप कुछ उदाहरण और सैंपल आउटपुट देख सकते हैं।

```Haskell
-- स्ट्रिंग की तुलना करें और एक string से दूसरे string को बदलें
replaceString :: String -> String -> String -> String
replaceString old new str = concatMap (\x -> if x == old then new else [x]) str
replaceString "Hello" "Hi" "Hello World" -- Output: "Hi World"

-- स्ट्रिंग की तुलना करें और एक string से दूसरे string को बदलें, केस अनुपालित न करें
replaceStringCaseIns :: String -> String -> String -> String
replaceStringCaseIns old new str = concatMap (\x -> if toLower x == toLower old then new else [x]) str
replaceStringCaseIns "hello" "Hi" "Hello World" -- Output: "Hi World"
```

## गहराईगामी जानकारी

हैस्केल में टेक्स्ट स्ट्रिंग की तुलना और बदलाव करने के लिए बहुत सारे तरीके हैं। हमने ऊपर केवल कुछ उदाहरण दिए हैं, लेकिन आप अपनी आवश्यकताओं के हिसाब से अन्य तरीकों का उपयोग कर सकते हैं। आप `Data.List` और `Data.Text` मॉड्यूल का उपयोग करके अधिक गहराई में जानकारी प्राप्त कर सकते हैं।

अतिरिक्त जानकारी के लिए, नीचे दिए गए लिंकों की सूची देखें:

## देखें

- [Haskell में स्ट्रिंग क
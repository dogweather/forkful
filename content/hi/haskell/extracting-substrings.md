---
title:    "Haskell: उप-स्ट्रिंग निकालना"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें एक स्ट्रिंग से अलग अलग उपस्थिति निकालने की आवश्यकता होती है, और इसके लिए हम substrings को निकाल सकते हैं। इस ब्लॉग पोस्ट में, हम हास्केल में यह कैसे कर सकते हैं उसके बारे में बात करेंगे।

## कैसे करें

हास्केल में substrings को निकालने के लिए हम `take` और `drop` फंक्शन का उपयोग कर सकते हैं। दोनों फंक्शन `:: Int -> [a] -> [a]` के टाइप हैं और पहला पैरामीटर उपस्थिति की संख्या होता है जिसको हम उचित उपस्थिति की मात्रा में भेज सकते हैं। नीचे हमने कुछ उदाहरण दिए हुए हैं।

```Haskell
str = "हास्केल में उपस्थितियाँ"

-- प्रथम ५ उपस्थितियाँ
take 5 str
> "हास्के"

-- पहले ६ उपस्थितियाँ हटा दें
drop 6 str
> "में उपस्थितियाँ"
```

## गहराई में जाएं

हम `take` और `drop` के साथ दो और फंक्शन का उपयोग करके भी substrings को निकाल सकते हैं। एक है `takeWhile` और दूसरा है `dropWhile`। ये फंक्शन स्ट्रिंग के आरम्भिक उपस्थितियों को खोजते हैं और उन्हें एक प्रेडिकेट से मिलते समय वापस करते हैं। नीचे हमने एक और उदाहरण दिया है।

```Haskell
numStr = "१२३४५६७८९०"

-- पहले अंकों को शामिल करें
takeWhile isDigit numStr
> "१२३४५६७८९"

-- पहले अंकों को हटाएं
dropWhile isDigit numStr
> "०"
```

## देखें भी

- [Haskell Documentation - take function](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:take)
- [Haskell Documentation - drop function](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:drop)
-
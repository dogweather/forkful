---
title:                "डिबग आउटपुट प्रिंट करना"
html_title:           "Haskell: डिबग आउटपुट प्रिंट करना"
simple_title:         "डिबग आउटपुट प्रिंट करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्यों

डीबग आउटपुट प्रिंट करने का उन्नत और उपयोगी तरीका है जो अपने कोड में त्रुटियों को सुधारने में मदद करता है। यह छोटी समस्याओं को ढूंढने और उन्हें ठीक करने में सहायता कर सकता है जो बड़े से बड़ी समस्याओं को रूक सकती हैं।

## कैसे करें

डीबग आउटपुट प्रिंट करने के लिए, आपको दो चीजें करनी होंगी। पहले, हासकेल में Debug.Trace मॉड्यूल को इम्पोर्ट करें। उसके बाद, आप "trace" फ़ंक्शन का उपयोग करके अपनी कोड में डीबग स्टेटमेंट हैंडल कर सकते हैं। नीचे कुछ उदाहरण दिए गए हैं।

```Haskell
import Debug.Trace

-- एक साधारण उदाहरण
factorial :: Int -> Int
factorial n = trace ("कल्पना: अभी हम n = " ++ show n ++ " के लिए factorial चलाएंगे") $ if n == 0 then 1 else n * factorial (n - 1)

factorial 5 -- कल्पना: अभी हम n = 5 के लिए factorial चलाएंगे

-- अलग-अलग स्थितियों में उपयोग
mean :: [Double] -> Double
mean xs = trace ("xs का अगला हिस्सा: " ++ show (head xs) ++ "\n" ++ "xs के शेष हिस्से: " ++ show (tail xs)) $ sum xs / (fromIntegral $ length xs)

mean [1, 2, 3] -- xs का अगला हिस्सा: 1.0
-- xs के शेष हिस्से: [2.0, 3.0]

mean [] -- xs का अगला हिस्सा: *** Exception: Prelude.head: empty list
```

## गहराई में जाओ

डीबग आउटपुट प्रिंट करने का एक और प्रभावी तरीका है ghci से डॉक्स प्रोम्प्ट पर या आपके कंपाइल किए गए फाइल में डीबग स्टेटमेंट लिखना। ये तरीके विकसित कोड में भी काम करते हैं और उन पर प्रकाश डीबगर का इस
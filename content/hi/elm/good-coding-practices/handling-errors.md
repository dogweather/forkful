---
title:                "एरर्स को हैंडल करना"
aliases: - /hi/elm/handling-errors.md
date:                  2024-01-26T00:51:44.266306-07:00
model:                 gpt-4-1106-preview
simple_title:         "एरर्स को हैंडल करना"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/handling-errors.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एरर्स को हैंडल करने का अर्थ है ऐसा कोड लिखना जो गलतियों की प्रत्याशा कर सके और उनसे निपट सके। प्रोग्रामर्स ऐसा क्रैशेस को रोकने, डाटा इंटेग्रिटी की रक्षा करने और यूज़र्स को कृपापूर्वक वैकल्पिक समाधान प्रदान करने के लिए करते हैं।

## कैसे करें:
Elm का मूल दर्शन है कोई रनटाइम अपवाद नहीं। इसलिए, Elm अपनी प्रकार (type) प्रणाली का उपयोग करता है, जैसे `Maybe` और `Result` प्रकारों के साथ, एरर्स को हैंडल करने के लिए।

`Maybe` परिदृश्य के लिए:

```Elm
safeDivide : Float -> Float -> Maybe Float
safeDivide numerator denominator =
    if denominator == 0 then
        Nothing
    else
        Just (numerator / denominator)
        
-- जब आप इसे चलाएंगे:

safeDivide 10 2
--> Just 5

safeDivide 10 0
--> Nothing
```

`Result` परिदृश्य के लिए:

```Elm
type Error = DivisionByZero

safeDivide : Float -> Float -> Result Error Float
safeDivide numerator denominator =
    if denominator == 0 then
        Err DivisionByZero
    else
        Ok (numerator / denominator)

-- और इसका उपयोग करते हुए:

safeDivide 10 2
--> Ok 5

safeDivide 10 0
--> Err DivisionByZero
```

## गहन अध्ययन
Elm की प्रकार प्रणाली सख्त होती है, जो समय से पहले एरर्स को पकड़ने में मदद करती है। ऐतिहासिक रूप से, अधिकांश भाषाओं ने अपवादों और रनटाइम चेक्स पर निर्भर किया था, परन्तु Elm ने संकलन-समय की गारंटियों का चयन किया। `Result` जैसे विकल्प विस्तृत एरर जानकारी देते हैं, जबकि `Maybe` हां-नहीं के परिदृश्यों के लिए सरल है। Elm की एरर हैंडलिंग प्रोग्रामर्स को सभी रास्तों पर पहले से विचार करने के लिए प्रोत्साहित करती है, भूले हुए एरर केसेस की पिटफॉल्स से बचने के लिए।

## और देखें:
- Elm की आधिकारिक गाइड का अनुभाग एरर हैंडलिंग पर: [एरर हैंडलिंग – एक परिचय](https://guide.elm-lang.org/error_handling/)
- Elm `Maybe` प्रलेखन: [Elm – Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe)
- Elm `Result` प्रलेखन: [Elm – Result](https://package.elm-lang.org/packages/elm/core/latest/Result)

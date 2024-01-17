---
title:                "टेस्ट लेखन"
html_title:           "Elm: टेस्ट लेखन"
simple_title:         "टेस्ट लेखन"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/writing-tests.md"
---

{{< edit_this_page >}}

--

हेल्लो Elm प्रोग्रामरों! विशेष रूप से जो प्रोग्रामिंग की दुनिया में नए हैं, उनके लिए आज हम आपको "Elm टेस्टिंग कैसे करें?" के बारे में बताने जा रहे हैं। तो चलिए शुरू करते हैं!

## क्या और क्यों?

पूर्ण ट्रांसपेरेंसी के मुद्दे से वेबवर्कर्स प्रत्येक दिन की तरह किसी न किसी एप्लिकेशन को बनाते हैं। इसमें जादा से जादा बग और अनुरूपताएं हो सकती हैं। लेकिन इन बगों को ध्यान रखने के लिए अनेक तरह की टेस्टिंग टूल्स उपलब्ध हैं। ये टूल्स आपको अपने कोड को परीक्षित करने में मदद करते हैं और सुरक्षित इल्म एप्लिकेशन बनाने में सहायता प्रदान करते हैं।

## कैसे करें?

```Elm
import Test exposing (..)

tests : Test
tests =
    describe "गणित कार्यशाला"
        [ test "2 और 3 का गुणन" <|
            \_ -> 2 * 3 |> Expect.equal 6
        , test "2 और 2 का जोड़" <|
            \_ -> 2 + 2 |> Expect.equal 4
        ]
```

यहाँ हम Elm कोड में अनुकूलन निष्पादित स्थितियों को परीक्षण करने के लिए Test पैकेज का उपयोग कर रहे हैं।

**उत्पाद उत्पन्न करने के लिए:**
```bash
elm-test
```

## गुहार

अगर आप अभी भी अपने लोकल वेबसाइट पर टेस्टिंग को सीखना चाहते हैं, तो इस ब्लॉग पोस्ट को जरूर देखें: [Elm: The Elm Test Package](https://www.learning-elm.com/introduction/testing/)

## अध्ययन करें


इस आर्टिकल में हमने आपको Elm कोड में टेस्टिंग करने का एक सरल तरीका दिखाया है। हालांकि, आप दूसरे टेस्टिंग फ्रेमवर्क्स के बारे में भी अधिक जान सकते हैं। यह फ्रेमवर्क्स आपको अपने कोड को और अधिक उत्पादित बनाने में मदद करते हैं।

## और जानें

- [Elm: The Elm Test Package](https://www.learning-elm.com/introduction/testing/)
- [Elm: The Test Framework Documentation](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Elm: The Official Guide for Testing](https://guide.elm-lang.org/testing/)
- [Elm: The Official Documentation](https://elm-lang.org/docs)
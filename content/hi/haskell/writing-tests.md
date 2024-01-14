---
title:    "Haskell: टेस्ट लिखना"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## क्यों

कंप्यूटर प्रोग्रामिंग के दुनिया में, बग्स और एरर्स से बचने के लिए, आपको टेस्टिंग विधियाँ लिखनी होगी। एक अच्छी टेस्ट स्वयं को एक प्रोग्राम को लंबे समय तक स्थिर रखने में और उसके लिए सुनिश्चित करने में मदद करती है।

## कैसे

```Haskell
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

-- एक साधारण टेस्ट केस
addTwoNumbersTest :: Test
addTwoNumbersTest = testCase "दो नंबरों को जोड़े" $
    assertEqual "अपेक्षित परिणाम" 5 (add 2 3)

-- TestSuite बनाएं और उसमें टेस्ट केस जोड़ें
tests :: TestSuite
tests = testGroup "सभी टेस्ट केस" [addTwoNumbersTest]

-- अपनी TestSuite चलाएं
main :: IO ()
main = defaultMain tests

-- Output:
-- दो नंबरों को जोड़े: OK (0 टेस्ट केस)
```

## गहराई में जाएं

टेस्ट केस विकास में अनूठा और महत्वपूर्ण है। आप अपनी प्रोग्राम के फंक्शन्स को मौजूदा और दुर्गम सितारों के लिए टेस्ट केस लिख सकते हैं। ये आपको प्रोग्रामिंग की गुणवत्ता एस्तिमेट करने में मदद करते हैं और आपको सुनिश्चित करते हैं कि कोड बदलाव के बाद भी आपका प्रोग्राम सही ढंग से काम कर रहा है।

## देखें भी

- [हैस्केल मैनुअल](https://www.haskell.org/documentation/)
- [HUnit डॉक्युमेंटेशन](https://hackage.haskell.org/package/HUnit/docs/Test-HUnit.html)
- [अतिरिक्त हैस्केल टेस्टिंग टूल्स](https://wiki.haskell.org/Testing_tools)
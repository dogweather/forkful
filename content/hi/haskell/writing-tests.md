---
title:                "परीक्षण लिखना"
html_title:           "Arduino: परीक्षण लिखना"
simple_title:         "परीक्षण लिखना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

टेस्ट लिखना मतलब आपके कोड का जाँच के लिए छोटे प्रोग्राम्स लिखना। यह काम इसलिए किया जाता है ताकि बग्स को पकड़ा जा सके और सुनिश्चित किया जा सके कि कोड में बदलावों के बाद भी सब कुछ सही काम करता रहे।

## How to: (कैसे करें:)

Haskell में टेस्ट लिखने के लिए `Hspec` एक पॉपुलर लाइब्रेरी है। यहाँ एक सिम्पल उदाहरण है:

```Haskell
import Test.Hspec
import Data.List (sort)

main :: IO ()
main = hspec $ do

  describe "sort" $ do
    it "sorts an empty list" $
      sort ([] :: [Int]) `shouldBe` ([] :: [Int])

    it "sorts a list of integers" $
      sort [3, 2, 1] `shouldBe` [1, 2, 3]
```

सैम्पल आउटपुट:

```
Test Suites: 1 passed, 1 total
Tests:       2 passed, 2 total
```

## Deep Dive (गहराई में जानकारी:)

Hspec की शुरूवात Haskell में टेस्टिंग को सरल बनाने के लिए की गयी थी। यह Behaviour Driven Development (BDD) पर आधारित है। QuickCheck और HUnit कुछ अन्य अल्टरनेटिव्स हैं जो कि प्रोपर्टी और यूनिट टेस्टिंग को कवर करते हैं। Hspec एक ह्यूमन-रीडेबल फॉर्मेट में टेस्ट केसेज लिखने की क्षमता देता है और इसे गुड प्रैक्टिस माना जाता है।

## See Also (और जानें:)

- [Hspec User's Manual](https://hspec.github.io/)
- [QuickCheck on Hackage](https://hackage.haskell.org/package/QuickCheck)
- [HUnit on Hackage](https://hackage.haskell.org/package/HUnit)
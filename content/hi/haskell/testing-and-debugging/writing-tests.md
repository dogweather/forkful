---
title:                "टेस्ट लिखना"
aliases:
- /hi/haskell/writing-tests/
date:                  2024-02-03T19:31:32.268824-07:00
model:                 gpt-4-0125-preview
simple_title:         "टेस्ट लिखना"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Haskell में परीक्षण लिखना आपके फंक्शन्स के उम्मीद के अनुसार काम करने की जाँच के लिए स्वचालित जाँच के माध्यम से सुनिश्चित करने के बारे में है। प्रोग्रामर इसे जल्दी बग्स को पकड़ने, रीफैक्टरिंग को सुविधाजनक बनाने, और व्यवहार को दस्तावेजीकरण करने के लिए करते हैं, जिससे कोडबेस अधिक रख-रखाव योग्य और स्केलेबल बनता है।

## कैसे करें:

Haskell विभिन्न टेस्टिंग फ्रेमवर्क का समर्थन करता है, लेकिन दो लोकप्रिय `Hspec` और `QuickCheck` हैं। Hspec आपको अपने कोड के लिए मानव-पठनीय विनिर्देशों को परिभाषित करने की अनुमति देता है, जबकि QuickCheck आपके कोड द्वारा संतुष्ट होने वाले गुणों का वर्णन करके स्वतः परीक्षणों को उत्पन्न करने देता है।

### Hspec का उपयोग करना

सबसे पहले, आपके बिल्ड टूल कॉन्फिगरेशन (जैसे, `stack.yaml` या `cabal` फ़ाइल) में `hspec` जोड़ें। फिर, `Test.Hspec` को आयात करें और विनिर्देशों के रूप में परीक्षण लिखें:

```haskell
-- फाइल: spec/MyLibSpec.hs
import Test.Hspec
import MyLib (add)

main :: IO ()
main = hspec $ describe "MyLib.add" $ do
  it "दो संख्याओं को जोड़ता है" $
    add 1 2 `shouldBe` 3

  it "शून्य जोड़ने पर पहली संख्या लौटाता है" $
    add 5 0 `shouldBe` 5
```

फिर, अपने बिल्ड टूल का उपयोग करके अपने परीक्षणों को चलाएं, जिससे एक आउटपुट निकल सकता है जैसे:

```
MyLib.add
  - दो संख्याओं को जोड़ता है
  - शून्य जोड़ने पर पहली संख्या लौटाता है

0.0001 सेकंड में समाप्त
2 उदाहरण, 0 विफलताएँ
```

### QuickCheck का उपयोग करना

QuickCheck के साथ, आप उन गुणों को व्यक्त करते हैं जो आपके फंक्शन्स को संतुष्ट करना चाहिए। अपनी परियोजना कॉन्फिगरेशन में `QuickCheck` जोड़ें, फिर इसे आयात करें:

```haskell
-- फाइल: test/MyLibProperties.hs
import Test.QuickCheck
import MyLib (add)

prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative x y z = x + (y + z) == (x + y) + z

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y = x + y == y + x

main :: IO ()
main = do
  quickCheck prop_addAssociative
  quickCheck prop_addCommutative
```

ये परीक्षण चलाने पर निर्दिष्ट गुणों की जाँच के लिए स्वतः इनपुट उत्पन्न होंगे:

```
+++ OK, पास हुए 100 परीक्षण।
+++ OK, पास हुए 100 परीक्षण।
```

Hspec और QuickCheck दोनों उदाहरणों में, परीक्षण सूट्‌स निष्क्रिय दस्तावेजीकरण के रूप में कार्य करते हैं जो आपके कोड की सहीता को स्वतः सत्यापित कर सकते हैं।

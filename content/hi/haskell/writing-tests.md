---
title:                "Haskell: टेस्ट लिखना"
simple_title:         "टेस्ट लिखना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/writing-tests.md"
---

{{< edit_this_page >}}

# क्यों

कोडिंग में उतनी जांच करना आवश्यक है जितनी उसे शुरू करना। इससे हम सुनिश्चित कर सकते हैं कि हमारा कोड सही तरीके से काम करता है और हमारे द्वारा किए गए बदलाव इसे बिगाड़ते नहीं हैं। इसलिए, अगर हम अपने हस्केल कोड को सुरक्षित बनाना चाहते हैं तो हमें उसे टेस्ट करना आवश्यक है।

## कैसे करें

इस ब्लॉग पोस्ट के माध्यम से हम सीखेंगे कि हस्केल में टेस्ट कैसे लिखें। हम आसान और स्पष्ट उदाहरण द्वारा समझेंगे कि कैसे हम अपने कोड के लिए विभिन्न प्रकार के टेस्ट लिख सकते हैं। हम अलग-अलग टेस्टिंग फ्रेमवर्क जैसे HUnit, QuickCheck और Hspec के बारे में भी जानेंगे। हम यह भी सीखेंगे कि कैसे हम टेस्ट कोड को मैन्टेन कर सकते हैं और एक स्वचालित टेस्ट स्यूट कैसे बना सकते हैं। संबन्धित कनेक्शन और मॉनैड्स का भी अध्ययन करेंगे।

इसके लिए, हम निम्नलिखित तरीकों का प्रयोग कर सकते हैं:

```Haskell
main :: IO ()
main = do
  -- पूर्वबद्ध सेटअप
  let name = "Alice"
  let age = 25
  let countries = ["India", "USA", "Canada"]
  
  -- टेस्ट लिखें
  testCase1 <- HUnit.testCase "उपयोगकर्ता का नाम सही है"
    (HUnit.assertEqual "उपयोगकर्ता का नाम गलत है" "Alice" name)

  testCase2 <- QuickCheck.testProperty "उपयोगकर्ता की उम्र गलत नहीं है"
    (\n -> age > 0 && age < 100)

  testCase3 <- Hspec.it "देशों की संख्या सही है"
    (Hspec.shouldBeEqualList countries ["India", "USA", "Canada"])

  -- टेस्ट स्यूट से ट
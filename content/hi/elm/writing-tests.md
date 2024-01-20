---
title:                "परीक्षण लिखना"
html_title:           "Arduino: परीक्षण लिखना"
simple_title:         "परीक्षण लिखना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/writing-tests.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
परीक्षण (Testing) सॉफ्टवेयर की जाँच करने का एक तरीका है. प्रोग्रामर इसलिए परीक्षण करते हैं जिससे बग्स का पता चल सके और सॉफ्टवेयर अच्छी तरह से काम करे.

## कैसे करें? (How to:)
Elm में परीक्षण (tests) लिखते समय `elm-test` पैकेज का उपयोग करते हैं. यहाँ एक बेसिक उदाहरण है: 

```Elm
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import String

testList : Test
testList = 
  describe "String Tests"
    [ test "length test" <| 
        \_ -> "hello" |> String.length |> Expect.equal 5
    , test "reverse test" <| 
        \_ -> "hello" |> String.reverse |> Expect.equal "olleh"
    ]

-- आपको उपयोग करने के लिए `elm-test` इंस्टॉल और चलाना होगा, फिर आप निचे दिए गए कमांड का उपयोग करके आपके टेस्ट्स चला सकते हैं:
-- elm-test
```

सैंपल आउटपुट:
```
TEST RUN PASSED

2 tests passed
```

## गहराई से जानकारी (Deep Dive)
Elm में परीक्षण लेखन की शुरुआत 'elm-test' पैकेज के साथ हुई थी. यह फंक्शनल प्रोग्रामिंग की विचारधारा को महत्व देते हुए बहुत सारे अलग-अलग परीक्षणों को चलाने की अनुमति देता है. इसके अल्टरनेटिव्स में Jest, Mocha जैसे JavaScript टेस्टिंग फ्रेमवर्क हैं लेकिन Elm के लिए हैं elm-test अधिक उपयुक्त है. elm-test का उपयोग करते समय, आपको फंक्शनल पैराडाइम्स का पालन करना होगा, और इसे इस्टॉल करने और अपडेट करने के प्रोसेस को समझना होगा.

## और भी देखें (See Also)
- elm-test पैकेज: [elm-explorations/test](https://package.elm-lang.org/packages/elm-explorations/test/latest)
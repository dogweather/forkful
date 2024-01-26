---
title:                "परीक्षण लिखना"
html_title:           "Arduino: परीक्षण लिखना"
simple_title:         "परीक्षण लिखना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
कोडिंग टेस्ट लिखना मतलब आपके प्रोग्राम के छोटे भागों की जाँच करना ताकि ये सुनिश्‍चित किया जा सके कि सब काम कैसे होना चाहिए। प्रोग्रामर इसलिए टेस्ट करते हैं क्योंकि इससे बग्स कम होती हैं, और कोड में बदलाव करना आसान हो जाता है।

## How to:
Fish Shell में आप `test` कमांड का इस्तेमाल करके टेस्ट केसेस लिख सकते हैं। नीचे एक सिंपल उदाहरण है:

```Fish Shell
function test_addition
  set -l sum (math 2 + 2)
  test $sum -eq 4; and echo "2 + 2 is $sum as expected."
end

test_addition
```

आउटपुट:

```
2 + 2 is 4 as expected.
```

अगर शर्त पूरी नहीं होती, तो `test` कमांड एक एरर कोड देता है।

## Deep Dive
Fish Shell का `test` कमांड POSIX शैली के `test` कमांड से अलग है। इसका इतिहास इसे और आसान और यूजर-फ्रेंडली बनाने के प्रयास से जुड़ा है। बजाय Bash और अन्य शेल्स के, Fish uses a more readable syntax. वहीं, टेस्टिंग के लिए आप Fish Shell के बाहर भी देख सकते हैं, जैसे कि `shunit2` या `bats` जो कि बश-कम्पैटिबल शेल्स के लिए हैं।

## See Also
Fish Shell के `test` कमांड के और डिटेल्स के लिए आप निम्न लिंक्स को देख सकते हैं:

- [Fish Shell Documentation for Conditions](https://fishshell.com/docs/current/commands.html#test)
- [Bats: Bash Automated Testing System](https://github.com/bats-core/bats-core)
- [shUnit2: Unit testing for shell scripts](https://github.com/kward/shunit2)

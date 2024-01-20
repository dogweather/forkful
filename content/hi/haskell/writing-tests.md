---
title:                "टेस्ट लिखना"
html_title:           "Haskell: टेस्ट लिखना"
simple_title:         "टेस्ट लिखना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

नए और अनुभवी प्रोग्रामर्स दोनों ही टेस्टिंग में मुख्य रूप से विश्वसनीय होना और कोड की गुणवत्ता में विश्वसनीयता को बनाए रखने के लिए टेस्टिंग करते हैं। हास्केल में टेस्टिंग मुख्य रूप से यूनिट टेस्टिंग और कंपोनेंट टेस्टिंग के दो भागों में होता है।

## कैसे करें:

```Haskell
-- उदाहरण १: पास केस

factorial 0 = 1
factorial n = n * factorial (n-1)

main = do
    print(factorial 5)

```
आउटपुट:
```
120
```

```Haskell
-- उदाहरण २: फेल केस

factorial n = if (n < 0) then error "गलत नंबर" else n * factorial (n-1)

main = do
    print(factorial (-5))
```
आउटपुट:
```
कॉलरर: गलत नंबर
```

## गहराई खोजें:

हास्केल में टेस्टिंग एक भूमिका निभाता है जो हमें कोड के गलती या बग को पता करने में मदद करता है। टेस्टिंग करने के कई अल्टरनेटिव हैं, जैसे यूनिट टेस्टिंग के लिए HUnit और QuickCheck और कंपोनेंट टेस्टिंग के लिए Selenium या Hspec। हास्केल में टेस्टिंग के लिए कई लाइब्रेरी भी हैं जैसे tasty और test-framework।

## अन्य जानकारी:

यदि आप हास्केल में टेस्टिंग करना चाहते हैं तो निम्नलिखित स्रोतों को देखें:

- [QuickCheck विकि पेज](https://wiki.haskell.org/Introduction_to_QuickCheck1)
- [Selenium विकि पेज](https://hackage.haskell.org/package/selenium)
- [Hspec विकि पेज](https://hspec.github.io/)
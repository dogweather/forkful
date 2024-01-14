---
title:                "Haskell: डिफाल्ट त्रुटि में लेखन"
simple_title:         "डिफाल्ट त्रुटि में लेखन"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्यों
आप स्टैंडर्ड त्रुटि को लिखने में शामिल होना क्यों चाहेंगे? आपके हस्केल प्रोग्राम में त्रुटियां हो सकती हैं और उन्हें सीधे दिखाने के लिए यह बहुत महत्वपूर्ण हो सकता है।

## कैसे
आप स्टैंडर्ड त्रुटि को लिखने के लिए `stderr` फ़ंक्शन का उपयोग कर सकते हैं। नीचे दिए गए कोड ब्लॉक में एक उदाहरण दिखाया गया है:

```Haskell
main :: IO ()
main = do
    hPutStrLn stderr "यह एक त्रुटि है।"
```
आपको निर्धारित मान पर उपयोग करके स्टैंडर्ड त्रुटि को लिख सकते हैं। इसका नतीजा नीचे प्रदर्शित है:
```
यह एक त्रुटि है।
```
आप इसका उपयोग करके अपने कोड में त्रुटियों को दिखा सकते हैं और इससे उन्हें सुधारने में मदद मिल सकती है।

## गहराई में जाएं
स्टैंडर्ड त्रुटि को लिखने के पीछे का परदा क्या है? आप सामान्यतया त्रुटियों को `throwIO` फ़ंक्शन का उपयोग करके मॉनैडियों में पेश करते हैं। इससे आपको स्टैंडर्ड त्रुटि को भी हैंडल करना होगा। आप हस्केल के IO मॉनैड को डोनबद के साथ मिलाने का उपयोग कर सकते हैं और इसका नतीजा हो सकता है:

```Haskell
main :: IO ()
main = do
    result <- try (hSetBuffering stdout NoBuffering)
    case result of
        Left e -> hPutStrLn stderr "त्रुटि हुईं।"
        Right _ -> putStrLn "स्थिति नॉर्मल है।"
```

## देखें भी
- [Haskell गाइड बुक](https://guide.haskell.org)
- [Haskell मैनुअल](https://www.haskell.org/documentation)
- [Haskell विकिआवधि](https://en.wikibooks.org/wiki
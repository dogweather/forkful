---
date: 2024-01-26 03:46:37.274739-07:00
description: "\u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u094B \u0917\
  \u094B\u0932 \u0915\u0930\u0928\u0947 \u0915\u093E \u0905\u0930\u094D\u0925 \u0939\
  \u0948 \u0909\u0928\u094D\u0939\u0947\u0902 \u0928\u093F\u0915\u091F\u0924\u092E\
  \ \u092A\u0942\u0930\u094D\u0923\u093E\u0902\u0915 \u092F\u093E \u0928\u093F\u0930\
  \u094D\u0926\u093F\u0937\u094D\u091F \u0926\u0936\u092E\u0932\u0935 \u0938\u094D\
  \u0925\u093E\u0928 \u0924\u0915 \u0938\u092E\u093E\u092F\u094B\u091C\u093F\u0924\
  \ \u0915\u0930\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\
  \u092E\u0930 \u0924\u094D\u0930\u0941\u091F\u093F\u092F\u094B\u0902 \u0915\u0947\
  \ \u0938\u0902\u091A\u092F \u0915\u094B \u0928\u094D\u092F\u0942\u0928\u0924\u092E\
  \ \u0915\u0930\u0928\u0947, \u092A\u0930\u093F\u0923\u093E\u092E\u094B\u0902\u2026"
lastmod: '2024-03-13T22:44:52.395750-06:00'
model: gpt-4-0125-preview
summary: "\u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u094B \u0917\u094B\
  \u0932 \u0915\u0930\u0928\u0947 \u0915\u093E \u0905\u0930\u094D\u0925 \u0939\u0948\
  \ \u0909\u0928\u094D\u0939\u0947\u0902 \u0928\u093F\u0915\u091F\u0924\u092E \u092A\
  \u0942\u0930\u094D\u0923\u093E\u0902\u0915 \u092F\u093E \u0928\u093F\u0930\u094D\
  \u0926\u093F\u0937\u094D\u091F \u0926\u0936\u092E\u0932\u0935 \u0938\u094D\u0925\
  \u093E\u0928 \u0924\u0915 \u0938\u092E\u093E\u092F\u094B\u091C\u093F\u0924 \u0915\
  \u0930\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\
  \ \u0924\u094D\u0930\u0941\u091F\u093F\u092F\u094B\u0902 \u0915\u0947 \u0938\u0902\
  \u091A\u092F \u0915\u094B \u0928\u094D\u092F\u0942\u0928\u0924\u092E \u0915\u0930\
  \u0928\u0947, \u092A\u0930\u093F\u0923\u093E\u092E\u094B\u0902\u2026"
title: "\u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u094B \u092A\u0942\
  \u0930\u094D\u0923\u093E\u0902\u0915 \u092C\u0928\u093E\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

संख्याओं को गोल करने का अर्थ है उन्हें निकटतम पूर्णांक या निर्दिष्ट दशमलव स्थान तक समायोजित करना। प्रोग्रामर त्रुटियों के संचय को न्यूनतम करने, परिणामों को उपयोगकर्ता प्रस्तुति के अनुरूप बनाने या तैरते-बिंदु ऑपरेशनों की गणना लागत को कम करने के लिए संख्याओं को गोल करते हैं।

## कैसे:

हास्केल `round`, `ceiling`, `floor`, और `truncate` फंक्शन्स का उपयोग `Prelude` से गोलीकरण ऑपरेशनों के लिए करता है।

```haskell
import Prelude

main :: IO ()
main = do
  let num = 3.567
  print $ round num    -- 4
  print $ ceiling num  -- 4
  print $ floor num    -- 3
  print $ truncate num -- 3
  
  -- एक विशेष दशमलव स्थान तक गोल करना Prelude में नहीं है।
  -- यहाँ एक अनुकूलित फंक्शन है:
  let roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
  print $ roundTo 1 num -- 3.6
```

## गहराई से विचार

ऐतिहासिक रूप से, संख्यात्मक विश्लेषण और कंप्यूटर विज्ञान में गोलाकार महत्वपूर्ण होता है क्योंकि गणनाओं में त्रुटि संचय को न्यूनतम करना महत्वपूर्ण होता है, विशेष रूप से इससे पहले कि फ्लोटिंग-पॉइंट प्रतिनिधित्व को IEEE 754 के साथ मानकीकृत किया गया था।

किस पर गोल करें? `round` आपको निकटतम पूर्णांक पर ले जाता है—ऊपर या नीचे। `ceiling` और `floor` हमेशा क्रमशः निकटतम पूर्णांक की ओर ऊपर या नीचे गोल करते हैं, जबकि `truncate` बस दशमलव बिंदुओं को छोड़ देता है।

इन फंक्शन्स के विकल्प में अनुकूलित तर्क, जैसे कि हमारा `roundTo`, शामिल हो सकता है, या आप अधिक जटिल आवश्यकताओं के लिए लाइब्रेरी (जैसे कि Data.Fixed) को खींच सकते हैं।

`round` में आधे रास्ते के मामलों को हास्केल कैसे संभालता है, इसके कारण अप्रत्याशित परिणामों के लिए सावधान रहें (यह निकटतम सम पूर्णांक को गोल करता है)।

## देखें भी

- गोलीकरण फंक्शन्स के लिए हास्केल प्रील्यूड दस्तावेजीकरण: https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html
- फ्लोटिंग-पॉइंट अंकगणित पर हास्केल विकी: https://wiki.haskell.org/Floating_point_arithmetic
- अधिकांश भाषाओं में फ्लोटिंग-पॉइंट को कैसे संभाला जाता है, इस पर IEEE 754-2008 मानक: https://ieeexplore.ieee.org/document/4610935

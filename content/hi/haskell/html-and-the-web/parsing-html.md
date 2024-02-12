---
title:                "HTML विश्लेषण"
date:                  2024-02-03T19:13:20.182343-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML विश्लेषण"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Haskell में HTML पार्स करना आपको डेटा निकालने, HTML सामग्री को संशोधित करने, या प्रोग्राम माध्यम से वेब पृष्ठों के साथ इंटरैक्ट करने की अनुमति देता है। वेब स्क्रैपिंग, वेब अप्लिकेशंस की स्वचालित परीक्षण, और वेबसाइटों से डेटा माइनिंग जैसे कार्यों के लिए यह संचालन अत्यावश्यक है - Haskell की मजबूत प्रकार प्रणाली और कार्यात्मक प्रोग्रामिंग पैराडाइम का लाभ उठाकर मजबूत और संक्षिप्त कोड सुनिश्चित करना।

## कैसे:

Haskell में HTML पार्स करने के लिए, हम इसकी साधारणता और लचीलेपन के लिए `tagsoup` लाइब्रेरी का उपयोग करेंगे। सबसे पहले, अपनी परियोजना की cabal फाइल में `tagsoup` जोड़कर या `cabal install tagsoup` चलाकर लाइब्रेरी को इंस्टॉल करना सुनिश्चित करें।

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup

-- प्रदर्शन के लिए नमूना HTML
let sampleHtml = "<html><body><p>Learn Haskell!</p><a href='http://example.com'>Click Here</a></body></html>"

-- HTML पार्स करें और लिंक्स (a tags) के लिए फ़िल्टर करें
let tags = parseTags sampleHtml
let links = [fromAttrib "href" tag | tag <- tags, isTagOpenName "a" tag]

-- निकाले गए लिंक्स प्रिंट करें
print links
```

नमूना आउटपुट:
```plaintext
["http://example.com"]
```

अधिक जटिल HTML पार्सिंग आवश्यकताओं के लिए, विशेष रूप से यदि आप दस्तावेज़ परिवर्तन के साथ काम कर रहे हैं, तो `pandoc` लाइब्रेरी का उपयोग करने पर विचार करें। यह असाधारण रूप से बहुमुखी है लेकिन अधिक जटिलता के साथ आता है:

```haskell
import Text.Pandoc

-- मान लें कि आपके पास एक Pandoc दस्तावेज़ (doc) लोड है, उदाहरण के लिए, एक फ़ाइल पढ़ने से
let doc = ... -- आपका Pandoc दस्तावेज़ यहाँ जाता है

-- दस्तावेज़ को HTML स्ट्रिंग में परिवर्तित करें
let htmlString = writeHtmlString def doc

-- अब, आप उपरोक्त के रूप में `htmlString` को पार्स करेंगे या अपनी आवश्यकताओं के अनुसार आगे बढ़ेंगे।
```
`pandoc` को ध्यान में रखें कि यह एक काफी बड़ी लाइब्रेरी है जो कई मार्कअप प्रारूपों के बीच परिवर्तन पर केंद्रित है, तो इसका उपयोग करें अगर आपको उन अतिरिक्त क्षमताओं की आवश्यकता है या यदि आप पहले से ही अपने एप्लिकेशन में दस्तावेज़ प्रारूपों से निपट रहे हैं।
---
title:                "HTML पार्स करना"
date:                  2024-01-20T15:32:12.746622-07:00
simple_title:         "HTML पार्स करना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? क्या और क्यों?
HTML पार्सिंग है वैब पेज के HTML कोड को समझकर उसकी संरचना को पहचानना। प्रोग्रामर्स इसे डेटा निकालने, वेब स्क्रेपिंग, या कंटेंट मॉडिफिकेशन के लिए करते हैं।

## How to: कैसे करें?
Haskell में HTML पार्सिंग के लिए, हम `tagsoup` लाइब्रेरी का इस्तेमाल करेंगे।

```Haskell
import Text.HTML.TagSoup

-- HTML डॉक्युमेंट को पार्स करने का फंक्शन
parseHtml :: String -> [Tag String]
parseHtml = parseTags

-- मुख्य फंक्शन जहाँ पार्सिंग होगी
main :: IO ()
main = do
    htmlContent <- readFile "example.html"
    let parsedHtml = parseHtml htmlContent
    print parsedHtml
```

सेम्पल `example.html` फाइल:

```html
<!DOCTYPE html>
<html>
<head>
    <title>मेरा टाइटल</title>
</head>
<body>
    <p>मेरा पैरा</p>
</body>
</html>
```

सेम्पल आउटपुट:

```plaintext
[TagOpen "!DOCTYPE" [("html","")],TagOpen "html" [],TagOpen "head" [],TagOpen "title" [],TagText "मेरा टाइटल",TagClose "title",TagClose "head",TagOpen "body" [],TagOpen "p" [],TagText "मेरा पैरा",TagClose "p",TagClose "body",TagClose "html"]
```

## Deep Dive: गहरी जानकारी
HTML पार्सिंग जरूरत होती है क्योंकि HTML डॉक्युमेंट अक्सर जटिल होते हैं और डेटा को मैन्युअली निकालना कठिन हो सकता है। ऐतिहासिक रूप से, पार्सर्स जैसे कि `tagsoup` ने इसे सरल बनाया है जो बहुत सारे स्ट्रक्चर के साथ काम कर सकता है, भले ही वे मानकों से पूरी तरह मेल न खाते हों। 

`tagsoup` के अलावा, प्रोग्रामर्स अक्सर `pandoc`, `hxt` या शायद `jsoup` (जो कि जावा के लिए है) जैसे अल्टरनेटिव्स का भी इस्तेमाल कर सकते हैं। महत्वपूर्ण बात यह है कि पार्सिंग लाइब्रेरी का चुनाव HTML द्वारा निर्धारित टास्क पर निर्भर करेगा। 

`tagsoup` गैर-सख्त पार्सिंग की प्रक्रिया अपनाता है, इसलिए यह त्रुटियों और अनुपालन की कमी वाले HTML के साथ भी काम कर सकता है। यह अक्सर वैब स्क्रैपिंग के लिए उपयोगी होता है जहाँ स्रोत कोड अनियमित हो सकता है।

## See Also: यह भी देखें
- `tagsoup` हैस्कल पैकेज: [Tagsoup on Hackage](https://hackage.haskell.org/package/tagsoup)

---
title:                "वर्तमान तारीख प्राप्त करें"
html_title:           "Elm: वर्तमान तारीख प्राप्त करें"
simple_title:         "वर्तमान तारीख प्राप्त करें"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

# क्यों

वर्तमान तारीख मिलाने में दिलचस्पी रखने के लिए लोगों को प्रेरित कर सकता है।

# कैसे

```Elm
import Time exposing (..)

currentTime : Int
currentTime =
    Time.now |> Time.millisecondSince1970

```

यहाँ, हमने Time लाइब्रेरी से Time मॉड्यूल को आयात किया है। इस मॉड्यूल में, हमें वर्तमान समय को मिलीसेकंड में प्राप्त करने के लिए Time.now और Time.millisecondSince1970 फंक्शन का उपयोग करना होता है। उदाहरण के लिए, यदि हम अपने concept फ़ाइल में ऊपर दिए गए कोड ब्लॉक को शामिल करते हैं, तो हमें निम्न आउटपुट मिलेगा:

> 1,619,268,486,199

यह मिलीसेकंड में प्राप्त हुआ मौजूदा की तारीख है। आप Time.millisecondToPosix फंक्शन का उपयोग करके इस तारीख को पोसिक्स समय में बदल सकते हैं।

# गहराई में

अधिक सटीक तारीख को प्राप्त करने के लिए, हम स्थान के आधार पर तारीख उत्पन्न कर सकते हैं। उदाहरण के लिए, यदि हम स्थान का नाम और समय क्षेत्र तुयें हैं, तो हम निम्न गहराई तक दायरा घोषित कर सकते हैं:

```Elm
import Time exposing (..)
import Time.Zone exposing (berlin)

berlinTime : Time
berlinTime =
    Time.now |> Time.inZone berlin
```

इस गहराई से, हम निम्न आउटपुट प्राप्त करेंगे:

> Time 2021 4 8 14 25 39315 131

इसमें, पहले 6 पैरामीटर दूसरे से 10 तक तारीख के विविध अंगों को दर्शाते हैं। उदाहरण के लिए, यदि हम इस गहराई के तारीख को अनुप्रयोग में मिस्त्रीयम के समय के अनुसार बदलने के लिए Time.millisecondToHour, Time.shift और Time.millisecondTo
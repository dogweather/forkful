---
title:                "स्ट्रिंग को कैपिटलाइज करना"
aliases: - /hi/elm/capitalizing-a-string.md
date:                  2024-02-03T19:05:55.328571-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

एक स्ट्रिंग को बड़े अक्षर में बदलना मतलब एक दी गई स्ट्रिंग के प्रारंभिक अक्षर को ऊपरी मामले (uppercase) में परिवर्तित करना जबकि बाकी को छोटे मामले (lowercase) में रखना, अक्सर मानकीकृत फॉर्मेटिंग या पढ़ने योग्यता के उद्देश्यों के लिए। प्रोग्रामर अक्सर यह कार्य करते हैं ताकि सुनिश्चित किया जा सके कि डेटा सुसंगत रूप से प्रस्तुत किया जाता है, विशेषकर उपयोगकर्ता इंटरफेस में या जब उपयोगकर्ता इनपुट की प्रक्रिया करते हुए और प्रदर्शित करते हुए।

## कैसे:

Elm में, स्ट्रिंग्स को बड़े अक्षर में बदलने के लिए विशेष रूप से कोई निर्मित फंक्शन नहीं है। हालांकि, आप `toUpper`, `toLower`, `left`, और `dropLeft` जैसे निर्मित `String` मॉड्यूल फंक्शनों का उपयोग करके आसानी से यह प्राप्त कर सकते हैं।

```elm
capitalize : String -> String
capitalize str =
    if String.isEmpty str then
        ""
    else
        String.toUpper (String.left 1 str) ++ String.toLower (String.dropLeft 1 str)

-- उदाहरण उपयोग
main =
    String.toList "hello world" |> List.map capitalize |> String.join " "
    -- आउटपुट: "Hello World"
```

अधिक जटिल परिदृश्यों के लिए या यदि आप सीधे तरीके से स्ट्रिंग्स को बड़े अक्षर में बदलने के लिए एक पुस्तकालय का उपयोग करना पसंद करते हैं, तो आप एक तृतीय-पक्ष पैकेज जैसे `elm-community/string-extra` पर विचार कर सकते हैं। हालांकि, मेरे आखिरी अपडेट के अनुसार, Elm का पारिस्थितिकी तंत्र भाषा और परियोजनाओं को लीन रखने के लिए ऐसे कार्यों को निर्मित फंक्शनों का उपयोग करके निपटाने का प्रोत्साहन देता है।

```elm
import String.Extra as StringExtra

-- यदि तृतीय-पक्ष पुस्तकालय में `capitalize` फंक्शन मौजूद हो
capitalizeWithLibrary : String -> String
capitalizeWithLibrary str =
    StringExtra.capitalize str

-- काल्पनिक पुस्तकालय फंक्शन के साथ उदाहरण उपयोग
main =
    "this is elm" |> capitalizeWithLibrary
    -- काल्पनिक आउटपुट: "This is elm"
```

यदि आप मानक पुस्तकालय से परे अतिरिक्त कार्यक्षमता की तलाश में हैं, तो हमेशा ताजा और सबसे पसंदीदा स्ट्रिंग मैन
िपुलेशन पुस्तकालयों के लिए Elm पैकेज रिपोजिटरी की जांच करें।

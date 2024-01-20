---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

तारिक़ों की तुलना नामक संकल्पना कंप्यूटर शास्त्र में दो विशिष्ट दिनांकों की तुलना वाली क्रमागत सन्दर्भ का निर्देशन करती है। प्रोग्रामर इसे अक्सर टश्टों को मैच करने, अद्यतितावस्थाओं को नियंत्रित करने या समय-आधारित कार्रवाई के लिए उपयोग करते हैं।

## कैसे करें:

किसी Elm प्राथमिकता को उत्तरार्ध, मानते हुए, आप ```elm``` कोड ब्लॉक के द्वारा दिनांकों की तुलना कर सकते हैं:

```elm
import Time

date1 : Time.Posix
date1 = Time.millisToPosix 1580515200000

date2 : Time.Posix
date2 = Time.millisToPosix 1596230400000

compareDates : Order
compareDates = compare date1 date2
```

इस कोड में `compareDates` का परिणाम LT (Less Than) होगा, इसका मतलब है कि `date1` `date2`से पहले है।

## गहरी गोताखोरी:

यद्यपि Elm की पीढ़ी 0.19 से, यहां ध्यान दिया गया है कि दिनांक तुलना कैसे की जाए। इसके विकल्प में JavaScript कोड शामिल हो सकते थे, लेकिन Elm में इसे विस्तार से कवर किया गया है। ऐसा करने का एक विकल्प Elm में तारीखों को `Time.Time`से `Int`में परिवर्तित करने का उपयोग करना होता है, जिससे संभवतः यथास्थान दृष्टांत निर्दिष्ट होता है।

## यह भी देखें:

खुद को और अधिक गहराता ले, पेनिक्स मिला संवादाता पैकेज के बारे में और जानने के लिए निम्नलिखित स्रोत हैं:

1. [Elm प्रशिक्षण सामग्री](https://github.com/dwyl/learn-elm)
2. [Elm दस्तावेज़ीकरण](https://package.elm-lang.org/packages/elm/time/latest/)
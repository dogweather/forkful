---
title:                "भविष्य या अतीत में एक तारीख की गणना करना"
html_title:           "TypeScript: भविष्य या अतीत में एक तारीख की गणना करना"
simple_title:         "भविष्य या अतीत में एक तारीख की गणना करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक तारीख को आगामी या भविष्य में आंकना तारीखिक विद्या का एक प्रमुख भाग है। प्रोग्रामर इसे कुछ स्पष्ट कामों के लिए करते हैं, जैसे कि समय-संशोधित डेटा के रूप में प्रयोग करना, या दिनांकों और वैधता की गणना करना।

## कैसे करें?
आप मास्टर टाइपस्क्रिप्ट में जाओ, आप आसानी से एक तारीख को भविष्य में या अगले तारीख में आगामी आंक सकते हैं। यहां एक उदाहरण है:

```TypeScript
let today = new Date();
let futureDate = new Date(today.getFullYear(), today.getMonth(), today.getDate() + 7);

console.log(futureDate);
```

इसका उत्पाद निम्नलिखित हो सकता है: 

```TypeScript
Sun Nov 29 2020 00:00:00 GMT+0530 (India Standard Time)
```

## गहराई तक जाएं
गवैंग ग्यान तक जाओ – 1) इतिहास में, और 2) विकल्पों में और 3) तारीख आंकन करने के आंशिक विवरण। आप एक तारीख को आगामी या भविष्य में कैसे जाँच सकते हैं और आपको अपने परियोजनाओं में इस कौशल का कैसे उपयोग कर सकते हैं, इससे अधिक जानने के लिए, [यहां](https://www.tutorialspoint.com/typescript/typescript_date_functions.htm) जरूर देखें।

## भी देखें
आप आगामी और भविष्य में तारीख को जाँचने के तरीकों के बारे में और भी अधिक जान सकते हैं। अपनी दैनिक तारीखिक विद्या और कोडिंग कार्य और परियोजनाओं में इसका उपयोग करें। शुभकामनाएं!
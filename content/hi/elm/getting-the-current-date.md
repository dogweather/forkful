---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
प्रोग्रामर्स "मौजूदा दिनांक प्राप्त करना" का उपयोग वास्तविक समय की जानकारी के लिए करते हैं – चाहे वह लॉग लिखना हो, संस्करणोत्तर जानकारी प्रदान करना हो, या समय-आधारित काम को नियंत्रित करना हो।

## कैसे?
मोजूदा दिनांक को Elm में प्राप्त करने के लिए नीचे दिए गए कोड का उपयोग करें।

```Elm
import Task exposing (Task)
import Time exposing (Posix)

getCurrentTime : Task x Posix
getCurrentTime =
    Time.now
```

यह कोड एक टास्क उत्पन्न करेगा जो मौजूदा दिनांक और समय को Posix में वापस करेगा।

## गहरी जानकारी
1. **ऐतिहासिक प्रकटीकरण**: पहली बार 1970 में Unix परिवार के तहत POSIX समय की परिभाषा परिचलित हुई थी, जिसे एल्म ने अपनाया है।
2. **विकल्प**: `Date` पैकेज भी है जिसका उपयोग मौजूदा दिनांक का प्रतिनिधित्व करने के लिए Elm में किया जाता है लेकिन इसे या `posix` का उपयोग करना व्यावसायिक अनुप्रयोगों में `posix` की अधिकता होती है।
3. **विन्यास विवरण**: अधिक शुद्धता के लिए, Elm में POSIX समय की उच्चतम समय-विन्यास शृंखला को समय घटकों के रूप में पदानुक्रमित किया जाता है।

## यदि आपको भी देखना हो
1. [Elm समय पुस्तिका](https://package.elm-lang.org/packages/elm/time/latest/)
3. [Unix समय की परिभाषा](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/now)
4. [POSIX समय निर्देशिका](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_343)
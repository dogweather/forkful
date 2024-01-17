---
title:                "अस्थायी फाइल बनाना"
html_title:           "Elm: अस्थायी फाइल बनाना"
simple_title:         "अस्थायी फाइल बनाना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक अस्थायी फ़ाइल बनाना, एक क्रिया है जो कई प्रोग्रामिंग भाषाओं में होती है। प्रोग्रामर अस्थायी फ़ाइल बनाते हैं ताकि वे अपने कोड को जाँच सकें या कोई अन्य काम कर सकें, जैसे कि फ़ाइलों को संपीड़ित करना। 

## कैसे करें:
कुछ तरीकों से एल्म में अस्थायी फ़ाइल बनाई जाती है, जैसे कि `File.temp` या `File.tempWith`, आप `elm/file` पैकेज से इन फंक्शंस को इंपोर्ट करके उन्हें इस्तेमाल कर सकते हैं। नीचे दी गई संबंधित उदाहरण पीछे के साथ एलम सिंटैक्स में हो सकती है।
```elm
import File

tempFile : File.Temp
tempFile =
    File.temp "mytempfile.txt"

tempWithFile : File.TempWith
tempWithFile =
    File.tempWith "mytempfile.txt" "600"
```

## गहराई में जाएं:
अस्थायी फ़ाइल बनाने के इतिहास का विस्तार, ऐल्टरनेटिव तरीके और अस्थायी फ़ाइल बनाने के विस्तृत अवलोकन के बारे में अधिक जानकारी जानने के लिए, आप नीचे दिए गए लिंक्स पर जाएं सकते हैं। 

## इससे भी देखें:
- [Elm पैकेज डॉक्यूमेंटेशन](https://package.elm-lang.org/packages/elm/file/latest/File)
- [स्टैकओवरफ्लो पोर्टल](https://stackoverflow.com/questions/tagged/elm)
---
title:                "Elm: टेस्ट लिखना"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/writing-tests.md"
---

{{< edit_this_page >}}

## क्यों

टेस्ट लिखने में क्यों लगाने की जरूरत है, यह काम क्यों महत्वपूर्ण है? अनुमानित कोड का परिणाम कैसे सुनिश्चित किया जाता है?

## कैसे

टेस्ट लिखना आसान है, फिर भी आपको कुछ मूल सक्रिय कोड की आवश्यकता है, विशिष्ट उदाहरण देखिए.

```Elm
import Html exposing (text)

-- function to add two numbers
add : Int -> Int -> Int
add x y =
    x + y

-- test cases 
addTest1 =
    add 2 3 == 5

addTest2 =
    add -1 8 == 7

addTest3 =
    add 0 0 == 0

-- output
text "Test 1 passed: " ++ toString addTest1 -- True
text "Test 2 passed: " ++ toString addTest2 -- True
text "Test 3 passed: " ++ toString addTest3 -- True
```

## गहराई से

टेस्ट लिखने के लिए कुछ अत्यधिक उत्पादक विचार और तकनीकें हैं। यह आपके कोड को मजबूत और दुरस्त बनाने में मदद कर सकता है। टेस्ट कोड का उपयोग करने से आपको अनुमानित कोड के बदलावों का सामना करने की आवश्यकता नहीं होगी। यह आपको सुरक्षित भावना देता है कि आपका सक्रिय कोड अपेक्षाकृत ढंग से काम करेगा।

## आपको देखना चाहिए

"अधिक जानकारी के लिए" शीर्षक से नेत्र समाप्त होता है और नीचे दिए गए लिंक आपको टेस्ट लिखने की और अधिक सहायता प्रदान करेंगे।

- [Elm डॉक्यूमेंटेशन](https://elm-lang.org/docs)
- [एल्म टेस्टिंग गाइड](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [टेस्ट के साथ एक ट्यूटोरियल](https://www.elm-tutorial.org/en/02-elm-arch/04-testing.html)
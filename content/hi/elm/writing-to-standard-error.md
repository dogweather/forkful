---
title:                "मानक त्रुटि में लिखना"
date:                  2024-01-19
html_title:           "Arduino: मानक त्रुटि में लिखना"
simple_title:         "मानक त्रुटि में लिखना"

category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Standard error (stderr) एक स्ट्रीम है जहाँ एरर मेसेज जाते हैं। प्रोग्रामर्स इसे डिबगिंग में मदद के लिए और नॉर्मल आउटपुट से एरर्स को अलग करने के लिए इस्तेमाल करते हैं।

## How to: (कैसे करें:)
Elm में सीधे stderr में लिखना संभव नहीं है, क्योंकि यह फ्रंटएंड लैंग्वेज है। हालाँकि, आप Elm में Ports के साथ JavaScript इंटरऑप्स का इस्तेमाल करके ऐसा कर सकते हैं:

```Elm
port module Main exposing (..)

-- Ports to send errors to JavaScript
port error : String -> Cmd msg

-- Example function that sends an error message
reportError : String -> Cmd msg
reportError errorMsg =
    error errorMsg
```

JavaScript कोड जो Elm से error port को सुने:

```javascript
app.ports.error.subscribe(function(errorMessage) {
  console.error(errorMessage);
});
```

इससे आपका Elm कोड `console.error` का इस्तेमाल करके ब्राउजर कि console में एरर दिखा सकता है।

## Deep Dive (गहराई में जानकारी):
Elm में stderr में लिखना सीधे संभव नहीं क्योंकि यह वेब ब्राउजर पर चलता है, और वेब APIs इस तरह के एक्सेस को प्रोवाइड नहीं करते। पारंपरिक प्रोग्रामिंग भाषाओं (जैसे C या Python) में, stderr का इस्तेमाल एक लॉन्ग-स्टैंडिंग कन्वेंशन है जो कि यूनिक्स के समय से है। एल्म डेवलपर्स Ports का इस्तेमाल करके JavaScript कोड से इंटरेक्ट कर सकते हैं जिससे वे ब्राउजर की सीमाओं को पार कर सकें।

## See Also (देखें भी):
- Elm के ports और interop फीचर्स के बारे में अधिक जानने के लिए: https://guide.elm-lang.org/interop/ports.html
- JavaScript console.error फंक्शन की और जानकारी के लिए: https://developer.mozilla.org/en-US/docs/Web/API/console/error
- सीधे stderr में लिखने का पारंपरिक तरीका: https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)

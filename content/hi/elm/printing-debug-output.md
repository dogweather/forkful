---
title:                "डबग आउटपुट प्रिंट करना"
html_title:           "Elm: डबग आउटपुट प्रिंट करना"
simple_title:         "डबग आउटपुट प्रिंट करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्यों

डीबग आउटपुट प्रिंट करने का सबसे आम मेथड है जो कि प्रोग्रामर्स को प्रोग्राम के विभिन्न हिस्सों में क्या हो रहा है का पता लगाने में मदद करता है। यह बग्स का पता लगाने और सही कोडिंग का अनुमान लगाने में आसानी पहुँच देता है।

## कैसे करें

```Elm
import Debug exposing (log)

myFunction : Int -> Int -> Int
myFunction x y = 
    let 
        sum = x + y 
    in 
        Debug.log "Adding numbers!" sum
```

### कोड विवरण

यहाँ, हमने `Debug` लाइब्रेरी से `log` फंक्शन को इम्पोर्ट किया है जो हमें दी गयी वैल्यू को प्रिंट करने में मदद करेगा। फिर हमने अपनी फंक्शन `myFunction` में पैरामीटर `x` और `y` को ले तथा उन्हें जोड़ कर `sum` में स्टोर किया है। और अंत में, हम `Debug.log` फंक्शन का प्रयोग करके अपनी कस्टम संदेश "Adding numbers!" के साथ `sum` वैल्यू को प्रिंट कर रहे हैं।

### उत्पादन

जब हम `myFunction 2 3` को कॉल करते हैं, तो हमें यह आउटपुट मिलेगा:

```
Adding numbers! 5
```

## गहराई में जाएं

डीबग आउटपुट की मदद से, हम परियोजना को आसानी से ट्रैक कर सकते हैं और समस्याओं को पकड़ने में भी मदद मिलती है। यदि आप अपने डीबग आउटपुट को ज्यादा वर्बोस देखना चाहते हैं, तो आप `Debug.toString` का भी इस्तेमाल कर सकते हैं जो आपको डाटा के घनत्वपूर्ण रूप को वर्बोस तरीके से दिखाएगा। आगे की विस्तृत जानकारी के लिए, [इस लिंक](https://guide.elm-lang.org/debugging/debugging.html) का इस्तेमाल क
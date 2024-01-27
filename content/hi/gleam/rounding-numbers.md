---
title:                "संख्याओं को पूर्णांक बनाना"
date:                  2024-01-26T03:46:51.372840-07:00
model:                 gpt-4-0125-preview
simple_title:         "संख्याओं को पूर्णांक बनाना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/rounding-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
संख्याओं की पूर्णांकन एक मूल्य को निकटतम निश्चित स्थान तक समायोजित करने के बारे में है—जैसे कि 2.56 को 3 में राउंड करना अगर हम पूर्ण संख्याओं तक राउंड कर रहे हों। प्रोग्रामर यह आसानी के लिए या कुछ संख्यात्मक विशेषताओं को पूरा करने के लिए करते हैं, आमतौर पर फ्लोटिंग-पॉइंट प्रेसिजन त्रुटियों के कारण होने वाली बारीकियों से बचने या आउटपुट को उपयोगकर्ता-अनुकूल बनाने के लिए।

## कैसे करें:
Gleam में, पूर्णांकन मानक पुस्तकालय में नहीं है मेरी आखिरी जाँच तक, लेकिन यहाँ पर आप आमतौर पर एक फ्लोट को निकटतम पूर्ण संख्या में राउंड कैसे करेंगे इसका विवरण है, Erlang कार्यों का सीधे उपयोग करके:

```gleam
external fn erlang_round(Float) -> Int = "erlang" "round"

pub fn main() {
  let rounded = erlang_round(2.56)
  rounded // आउटपुट: 3
}
```

आउटपुट:
```
3
```

दिमाग में कुछ और प्रेसिजन है? जैसे, दो दशमलव स्थानों तक राउंडिंग? हमें थोड़ी गणित की जरूरत है:

```gleam
pub fn round_to_two_places(num: Float) -> Float {
  let multiplier = 100.0
  let tmp = num * multiplier
  let round_tmp = erlang_round(tmp)
  round_tmp / multiplier
}

pub fn main() {
    round_to_two_places(2.569) // आउटपुट: 2.57
}
```

आउटपुट:
```
2.57
```

## गहन अध्ययन
ऐतिहासिक रूप से, संख्याओं की पूर्णांकन महत्वपूर्ण रही है, खासकर वित्तीय और वैज्ञानिक गणनाओं में जहां सटीकता और मानक बहुत मायने रखते हैं। पूर्णांकन के बिना, आपको चारों ओर बदसूरत लंबे दशमलव मिलेंगे, जिससे गणनाएँ हाथ से निकल जाएंगी और त्रुटि के प्रति संवेदनशील हो जाएंगी।

प्रोग्रामिंग दुनिया में, विभिन्न भाषाएँ विभिन्न तरीके प्रस्तावित करती हैं, बिल्ट-इन फंक्शन से लेकर व्यापक गणित पुस्तकालयों तक। पूर्णांकन में विभिन्न नियम शामिल हो सकते हैं - उदाहरण के लिए, "राउंड हाफ अप" (सामान्य विधि) या "राउंड हाफ टू ईवन" (अक्सर वित्तीय गणनाओं में पक्षपात से बचने के लिए उपयोग किया जाता है)।

Gleam, Erlang की जड़ों वाली एक युवा भाषा होने के नाते, Erlang के रोबस्ट संख्यात्मक कार्यों पर निर्भर करती है। जैसा कि भाषा बढ़ती है, हमें देखने को मिल सकता है कि नेटिव फंक्शन पेश किए जाते हैं, बाहरी दिनचर्याओं को कॉल करने की आवश्यकता को कम करते हुए।

## इसे भी देखें
- अधिक संख्या कुचलने के लिए Erlang की :math मॉड्यूल: https://erlang.org/doc/man/math.html
- पूर्णांकन क्यों चुनौतीपूर्ण हो सकता है, इस पर पृष्ठभूमि के लिए, IEEE फ्लोटिंग पॉइंट स्टैंडर्ड: https://ieeexplore.ieee.org/document/8766229
- इसके पीछे की गणित में दिलचस्पी है? " हर कंप्यूटर वैज्ञानिक को फ्लोटिंग-पॉइंट अरिथमैटिक के बारे में जानना चाहिए" देखें: https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
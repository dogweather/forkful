---
title:                "स्ट्रिंग से तारीख का पार्सिंग"
html_title:           "Kotlin: स्ट्रिंग से तारीख का पार्सिंग"
simple_title:         "स्ट्रिंग से तारीख का पार्सिंग"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या है और क्यों?

डेट स्ट्रिंग से डेट को पार्स करना एक आम कार्य है जो डेट और समय को किसी स्ट्रिंग से पढ़ने और उसे इनपुट फॉर्मेट में बदलने को कहता है। प्रोग्रामर इसे अपने कोड में समय और तिथि को लागू करने के लिए करते हैं। 

## कैसे:

```Kotlin
// एक सामान्य स्ट्रिंग से डेट का पार्स करें
val input = "06/23/2021"
val formatter = SimpleDateFormat("MM/dd/yyyy")
val date = formatter.parse(input)

//उपयोगकर्ता को दिनांक का अलग अंश लेने के लिए
val day = date.getDate()

//डेट स्ट्रिंग से समय का पार्स करें
val inputTime = "05:30 PM"
val formatter = SimpleDateFormat("hh:mm a")
val time = formatter.parse(inputTime)
```

## गहराई में जाओ:

डेट स्ट्रिंग से डेट को पार्स करने की शुरूआत उस समय के साथ हुई जब प्रोग्रामरों को अपने कोड में समय और तिथि को बदलने की जरूरत महसूस हुई। आज, डेट स्ट्रिंग से डेट को पार्स करना एक आम कार्य है और उसे कई तरीकों से किया जाता है। एक अलग परिवर्तन का एक ऊँची स्तर पर उपयोग एक कस्टम डेट पार्सर बनाने का है जो उपयोगकर्ताओं को अपने विशिष्ट तारिकों और फॉर्मेट मैचर के साथ समय और तिथि को पार्स करने की अनुमति देता है। आप अपने कोड में डेट श्रृंखला, कॉलेंडर, या उपयोगकर्ता द्वारा शब्दों में दिए गए समय और तिथि को पार्स करने के लिए भी उपयोग कर सकते हैं।

## इससे सम्बंधित देखें:

- [SimpleDateFormat मानक डेट पार्सर को उपयोग करने का पूरा तरीका](https://developer.android.com/reference/java/text/SimpleDateFormat)
- [कैसे डेट स्ट्रिंग से समय को पार्स करें - विकि हाउ](https://hi.wikihow.com/%E0%A4%A1%E0%A5%87%E0%A4%9F-%E0%A4%AF%E0%A5%82%E0%A4%B0%E0%A4%B5%E0%A4%BE%E0%A4%8A-%E0%A4%B8%E0%A5%87-%E0%A4%B8%E0%A4%AE%E0%A4%AF-%E0%A4%95%E0%A5%8B-%E0%A4%AA%E0%A4%BE%E0%A4%B0%E0%A5%8D%E0%A4%B8-%E0%A4%95%E0%A4%B0%E0%A5%87%E0%A4%82)
- [कोटलिन में डेट को पार्स करना - जावा डेवलपर्स हिंदी में](https://www.javadevjournal.com/kotlin/how-to-parse-dates-in-kotlin/)
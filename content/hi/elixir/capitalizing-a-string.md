---
title:    "Elixir: स्ट्रिंग को बड़े अक्षरों में लिखना"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## क्यों

बहुत सारे एप्लिकेशन में हम देखते हैं कि टेक्स्ट की शुरुआत जगहों पर ही कैपिटल लेटर्स से होती है। ऐसी स्थितियों में हमें टेक्स्ट के शुरुआती अक्षरों को कैपिटलाइज करने की आवश्यकता होती है। यह देखने में भले ही आसान लगे, लेकिन इसका लोजिक एक बार थोड़ा गहराई से समझ लेने से ही हम इसे बेहतरीन तरीके से इम्प्लीमेंट कर सकते हैं।

## कैसे

इलिक्सिर में टेक्स्ट को कैपिटलाइज करने के लिए हम सरल तरीके से `String.capitalize/1` फंक्शन का इस्तेमाल कर सकते हैं। यह फंक्शन दो पैरामीटर लेता है, जो हमें टेक्स्ट और कैपिटलाइज करने की समझ देता है। नीचे दिए गए कोड ब्लॉक में आप `String.capitalize/1` का सही उपयोग देख सकते हैं।

```Elixir
text = "hello world"
String.capitalize(text) #=> "Hello world"
```

अगर हम कोड पर नज़र डालें, तो हम देखेंगे कि यह `String.capitalize/1` फंक्शन आसानी से हमारी जरूरतों को पूरा करता है। इसे यूज़ करना भी बहुत ही आसान है।

## गहराई में

इस फंक्शन का इस्तेमाल करने से पहले हमें समझ लेना जरूरी है कि यह आखिर हमें क्या देखने वाला है। यह फंक्शन स्ट्रिंग के प्रथम अक्षर को कैपिटलाइज करता है और बाकी सभी अक्षरों को लोअर केस में छोड़ देता है। लेकिन अगर हमें कोई अन्य लोजिक को होमेज़ देखना है तो हम `String.capitalize/2` फंक्शन का इस्तेमाल कर सकत
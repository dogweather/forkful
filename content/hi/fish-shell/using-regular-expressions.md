---
title:    "Fish Shell: रेगुलर एक्सप्रेशन का उपयोग करना"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## क्यों

आमतौर पर, रेगुलर एक्सप्रेशन्स का उपयोग संदर्भों में विशिष्ट टेक्स्ट पैटर्न को खोजने और फ़िल्टर करने के लिए किया जाता है। ये काम खासकर विशेषज्ञ प्रोग्रामिंग भाषाओं के उपयोग में किया जाता है जहाँ की साधारण स्ट्रिंग मैचिंग का उपयोग अधिक असुरक्षित हो सकता है। और इस रूप में रेगुलर एक्सप्रेशन्स एक बहुत ही उपयोगी और शक्तिशाली उपकरण हैं।

## कैसे करें

फिश शेल में रेगुलर एक्सप्रेशन्स का उपयोग बहुत ही आसान है। नीचे दिए गए कोड ब्लॉक को ध्यान से देखें और उनमें दिए गए उदाहरणों को देखें। 

```
Fish Shell में रेगुलर एक्सप्रेशन द्वारा स्ट्रिंग मैच करें:

> string match "hello" "hello world"

हम एक वयंजन का उपयोग कर सकते हैं जो गुजराती, हिंदी, जापानी आदि भाषाओं में ध्वनि वर्णों के समूह को मैच करता है:

> string match --invert --regex '\w' "മല" "കല"

नीचे दिए गए उदाहरण में हम एक फ़ाइल से सभी लाइनों को मैच करेंगे जो पदक शब्द से आरंभ होते हैं और `medal.txt` के रूप में सेव किए जाएंगे:

> grep "^Padak" example.txt > medals.txt
```

## गहराई में जाने

अगर आप गहराई में जाना चाहते हैं कि रेगुलर एक्सप्रेशन्स कैसे काम करते हैं और क्या अलग-अलग सीमाओं के लिए उपयोग किए जा सकते हैं, तो आप फिश शेल की आधिकारिक डॉक्यूमेंट
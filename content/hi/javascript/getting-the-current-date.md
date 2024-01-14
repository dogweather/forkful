---
title:    "Javascript: वर्तमान तारीख प्राप्त करना"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# क्यों

कभी-कभी आपके पास कुछ काम की तारीख को जानने की आवश्यकता होती है, जैसे कि अपने ब्लॉग पोस्ट के प्रकाशन तारीख को दर्शाने के लिए या अपने कैलेंडर में इस हफ्ते कोनसे दिन हैं। इस स्थिति में, एक जावास्क्रिप्ट प्रोग्राम आपके लिए उपयोगी साबित हो सकता है।

# कैसे करें

क्या आप जानते हैं कि आप जावास्क्रिप्ट में सामान्य तारीख को प्राप्त करने के लिए ```Date()``` फंक्शन का प्रयोग कर सकते हैं? इसका उपयोग करके, आप क्लाइंट कंप्यूटर की आधार पर वर्तमान दिनांक और समय प्राप्त कर सकते हैं। 

इसके लिए, आपको निम्नलिखित स्टेप्स को दोहराना होगा:

1. सबसे पहले, ```Date()``` फंक्शन को उपयोग करके एक नया दिनांक ऑब्जेक्ट बनाएं। यह वर्तमान समय और तिथि को आपके स्क्रिप्ट में दर्शाता है। जैसे: ```var current_date = new Date();```

2. फिर, आप current_date ऑब्जेक्ट के लिए ```getFullYear()```, ```getMonth()```, ```getDate()```, ```getHours()```, ```getMinutes()```, और ```getSeconds()``` जैसे फंक्शन्स का उपयोग अपनी आवश्यकतानुसार कर सकते हैं। जैसे: ```var current_year = current_date.getFullYear();```

आपका पूर्वावलोकन स्थिति कुछ इस तरह का होना चाहिए:

```
var current_date = new Date();
var current_year = current_date.getFullYear();
var current_month = current_date.getMonth() + 1;
var current_day = current_date.getDate();
var current_hours = current_date.getHours();
var current_minutes = current_date.getMinutes();
var current_seconds = current_date.getSeconds();
```

# गहराई में

इस तरह से, आप current_date ऑब्जेक्ट को अपनी आवश्यकतानुसार तारीख, समय, और समय-स्तम्भ को प्राप्त कर सकते हैं। इसमें हमन
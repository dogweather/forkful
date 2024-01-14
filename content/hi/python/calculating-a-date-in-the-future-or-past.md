---
title:    "Python: Bhavishya ya Bhoot mein ek taarikh ka hisaab karna"
keywords: ["Python"]
---

{{< edit_this_page >}}

आपने कभी सोचा है कि भविष्य में कितने दिनों में आपका जन्मदिन है या पिछले साल कब आपका सालगिरह था? अगर हां, तो आप अपने Python कोड का उपयोग करके आसानी से इसका पता लगा सकते हैं। 

## क्यों

किसी भी व्यक्ति के लिए शायद उनके आने वाले जन्मदिन या सालगिरह का पता लगाना उत्सुकता का विषय नहीं हो सकता है, लेकिन एक अच्छे Python प्रोग्रामर के लिए यह एक चुनौतीपूर्ण स्किल हो सकती है। आप अपने Python कोड का उपयोग करके आसानी से गुजरे हुए तिथियों की गणना कर सकते हैं, जो किसी आवश्यक प्रोजेक्ट के लिए बहुत उपयोगी हो सकता है। 

## कैसे करें

```Python
# वर्तमान दिनांक का पता लगाएं
from datetime import date
today = date.today()
print("आज की तारीख:", today)

# 15 दिनों में भविष्य में की तारीख का पता लगाएं
from datetime import timedelta
future_date = today + timedelta(days=15)
print("15 दिनों में भविष्य में की तारीख:", future_date)

# पिछले साल का जन्मदिन की तारीख का पता लगाएं
import calendar
birthday = date(1995, 8, 10)
last_year = today.replace(year = today.year - 1)
print("पिछले साल की जन्मदिन की तारीख:", last_year)

# भविष्य में दिन और महीने की गणना करें
days = 300
months = days/30
print("गणना की गई महीनों:", months)
```

```
आज की तारीख: 2021-01-01
15 दिनों में भविष्य में की तारीख: 2021-01-16
पिछले साल की जन्मदिन की तारीख: 2020-08-10
गणना की गई महीनों: 10.0
```

## Deep Dive

यदि आप अपने Python कोड का और अपनी स्किल को बेहतर बनाना चाह
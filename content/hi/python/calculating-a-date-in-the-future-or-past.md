---
title:                "Python: भविष्य या भूत की तारीख की गणना"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों
अगले या पिछले दिनांक की गणना को करने में क्यों लग जाना उपयोगी हो सकता है।

## कैसे करें
दिनांक की गणना करने के लिए, हम अनेक तरीकों का प्रयोग कर सकते हैं। कुछ मुख्य तरीके निम्नांकित हैं:
```Python
# आगामी 10 दिन का दिनांक प्राप्त करने के लिए
import datetime
today = datetime.date.today()
future_date = today + datetime.timedelta(days=10)
print("आगामी 10 दिनों का दिनांक:", future_date)

# पिछले 10 दिनों का दिनांक प्राप्त करने के लिए
import datetime
today = datetime.date.today()
past_date = today - datetime.timedelta(days=10)
print("पिछले 10 दिनों का दिनांक:", past_date)

# दिनांक को तारीख में परिवर्तित करने के लिए
import datetime
date_str = "10-12-2023"
date = datetime.datetime.strptime(date_str, "%d-%m-%Y").date()
print("तारीख:", date)
```

आप अपनी आवश्यकतानुसार इन तरीकों का प्रयोग कर सकते हैं और अपनी प्रोजेक्ट में दिनांक की गणना के लिए इनका उपयोग कर सकते हैं।

## गहरा अध्ययन
अधिक जानकारी के लिए, दिनांक की गणना के लिए मूल्यवान साधनों के बारे में आप और जान सकते हैं। जैसे कि Python में उपलब्ध लाइब्रेरी `datetime` और `calendar` हैं जो दिनांक की गणना के लिए उपयोगी हो सकते हैं। आप इनका भी अध्ययन कर सकते हैं और अपने कोड में उन्हें शामिल करके दिनांक की गणना को और अधिक अनुकूल बना सकते हैं।

## देखें भी
अगर आपको Python के अन्य गाइड और ट्यूटोरियल्स की तलाश है तो आप निम्नलिखित लिंक्स की सहायता ले सकते हैं:

- [आसान Python शुरू करें (ह
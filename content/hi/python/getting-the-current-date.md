---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "Python: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों

आज की तारीख को प्राप्त करने की जरूरत हमारे दैनिक जीवन और डेटा प्रसंस्करण के लिए बहुत महत्वपूर्ण है। इससे हमारे कोड में विभिन्न तारीखों और समयों को प्रबंधित करने के लिए आसानी से इस्तेमाल किया जा सकता है।

## कैसे करें

आप Python में `datetime` मॉड्यूल का उपयोग करके आज की तारीख को प्राप्त कर सकते हैं। नीचे दिए गए उदाहरण में, हम `datetime` से `date` और `today()` फंक्शन का उपयोग करके आज की तारीख को प्राप्त कर रहे हैं।

```Python
# आज की तारीख को प्राप्त करें
import datetime

today = datetime.date.today()

print("आज की तारीख:", today)
```

आप अपने कोड में इस तरह से आज की तारीख को प्राप्त कर सकते हैं। अगर आप आज के अलावा अन्य तारीखों को भी प्राप्त करना चाहते हैं तो आप `day`, `month` और `year` जैसे अन्य फंक्शन भी इस्तेमाल कर सकते हैं।

## डीप डाइव

एक्सट्रा पैरामीटर सेट करने से आप `today()` फंक्शन का उपयोग करके अलग-अलग तारीखों को प्राप्त कर सकते हैं। आप अपने कोड में यह तरीका इस्तेमाल करके आज के साथ और कुछ दिनों के साथ अपनी तारीखों को भी प्रबंधित कर सकते हैं।

```Python
# एक्सट्रा पैरामीटर सेट करने से आज के बाद की तारीख प्राप्त करें
import datetime

today = datetime.date.today()

# एक दिन बाद की तारीख
tomorrow = today + datetime.timedelta(days=1)

# एक हफ्ते बाद की तारीख
next_week = today + datetime.timedelta(weeks=1)

print("आज की तारीख:", today)
print("कल की तारीख:", tomorrow
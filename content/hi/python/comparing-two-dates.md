---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

दो तारीखों की तुलना का मतलब है कि देखा जाता है कि कौन सी तारीख पहली है और कौन सी दूसरी. इसे प्रोग्रामर्स तब करते हैं जब उन्हें तारीखों की क्रमबद्धता या अवधि का पता लगाना होता है.

## कैसे करें:

```Python
from datetime import datetime

# दो तारीखें निर्धारित करें
dateA = datetime(2021, 5, 17)
dateB = datetime(2021, 7, 21)

# तारीखों की तुलना करें
if dateA > dateB:
    print("DateA is later than DateB")
elif dateA < dateB:
    print("DateA is earlier than DateB")
else:
    print("Both dates are identical")
```

सैंपल आउटपुट:

```Python
DateA is earlier than DateB
```

## गहरा अन्वेषण:

कंप्यूटर साइंस में तारीख की गणना और तुलना बहुत पुरानी अवधारणा है। ऐसा इसलिए होता है क्योंकि प्रोग्रामों को अक्सर तारीखों के बीच की अवधि की गणना करनी पडती है या एक कार्य क्रम निर्धारित करने के लिए तारीखों की तुलना करनी पडती है।

Python में तारीखों की तुलना करने का कई तरीके हैं। उदाहरण के लिए, आप pandas और numpy जैसी लाइब्ररीज़ का उपयोग कर सकते हैं। वैसे ही, आप गर प्रतिशत की गणना करनी हो, तो आप तारीख के दिन, महीने, और वर्ष के हिस्सों की तुलना कर सकते हैं। 

## और भी देखें:

1. [Python कोर डॉक्युमेंटेशन: datetime](https://docs.python.org/3/library/datetime.html)
3. [StackOverflow: तारीखों की गहरी तुलना](https://stackoverflow.com/questions/39533247/compare-two-dates)
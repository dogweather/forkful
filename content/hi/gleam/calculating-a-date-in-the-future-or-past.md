---
title:                "Gleam: भविष्य में या भूतकाल में तारीख की गणना"
simple_title:         "भविष्य में या भूतकाल में तारीख की गणना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों

आजकल हमारे जीवन का शेड्यूल अत्यंत जटिल हो गया है और हमें हमारे दैनिक कार्यों को प्रबंधित करने के लिए अनेक टूल्स की आवश्कता पड़ती है। ऐसे में भविष्य में या भूतकाल में एक विशिष्ट तिथि की गणना करने की जरूरत पड़ सकती है। इसकी सहायता से हम अपने शेड्यूल को बेहतर ढंग से प्रबंधित कर सकते हैं।

## कैसे करें

```Gleam
let today = Date.today()
let futureDate = today.add(days: 30)
let pastDate = today.subtract(years: 2, months: 6)
```

ऊपर दिए गए कोड ब्लॉक में हमने Gleam प्रोग्रामिंग भाषा का उपयोग करके आज की तारीख से 30 दिन आगे और 2 साल 6 महीने पहले की तारीख की गणना की है। इस तरह से हम आसानी से भविष्य में या भूतकाल में कोई भी तिथि की गणना कर सकते हैं। इसके अलावा, हम विभिन्न मात्राओं का उपयोग करके वर्षों, महीनों और दिनों को भी जोड़ सकते हैं।

## विस्तार से जानें

Date लाइब्रेरी में हमें भविष्य और भूतकाल की तिथियों को गणना करने के लिए कई अन्य फंक्शन भी मिलते हैं। इनमें से कुछ हैं - `add_years()`, `add_months()`, `add_weeks()`, `add_days()`, `subtract_years()`, `subtract_months()`, `subtract_weeks()`, `subtract_days()` आदि। इन फंक्शन को उपयोग करके हम अपनी तिथि को और भी फ्लेक्सिबल बना सकते हैं।

## आगे देखें

[Date लाइब्रेरी डॉक्यूमेंटेशन](https://gleam.run/documentation/stdlib/date.html)
[Gleam प्रोग्रामिंग भाषा](https://gleam.run/)
[Date गणना की वैज्ञानिक विधि](https://en
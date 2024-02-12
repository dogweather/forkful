---
title:                "वर्तमान तारीख प्राप्त करना"
aliases:
- /hi/python/getting-the-current-date.md
date:                  2024-02-03T19:11:39.540349-07:00
model:                 gpt-4-0125-preview
simple_title:         "वर्तमान तारीख प्राप्त करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Python में वर्तमान दिनांक प्राप्त करना कई अनुप्रयोगों के लिए एक मूलभूत कार्य है, जैसे कि लॉगिंग, डेटा विश्लेषण, और समय-आधारित निर्णय लेना। यह सिस्टम के वर्तमान दिनांक को प्राप्त करने के बारे में है, जो समय पर आधारित कार्यों के लिए महत्वपूर्ण है।

## कैसे:

**मानक पुस्तकालय `datetime` का उपयोग करके:**

Python की मानक पुस्तकालय में `datetime` मॉड्यूल तिथियों और समयों को संभालने के लिए कक्षाएं प्रदान करता है। वर्तमान दिनांक प्राप्त करने के लिए, आप `date.today()` विधि का उपयोग कर सकते हैं।

```python
from datetime import date

today = date.today()
print(today)  # परिणाम: YYYY-MM-DD (उदाहरण के लिए, 2023-04-05)
```

**समय स्वरूपण:**

यदि आपको वर्तमान दिनांक एक विभिन्न प्रारूप में चाहिए, तो `strftime` विधि आपको कस्टम दिनांक स्वरूपण निर्दिष्ट करने की अनुमति देती है:

```python
from datetime import date

today = date.today()
formatted_date = today.strftime('%B %d, %Y')  # उदाहरण प्रारूप: "अप्रैल 05, 2023"
print(formatted_date)
```

**अधिक लचीलेपन के लिए `pendulum` का उपयोग (एक लोकप्रिय तीसरे पक्ष की लाइब्रेरी):**

`Pendulum` एक तीसरे पक्ष की लाइब्रेरी है जो Python में तिथियों और समयों के साथ काम करने के लिए एक अधिक सहज दृष्टिकोण प्रदान करती है। यह मानक datetime कार्यक्षमताओं का विस्तार करती है और समय क्षेत्र प्रबंधन को सरल बनाती है, इसके अन्य विशेषताओं के बीच में।

पहले, सुनिश्चित करें कि आपने pip के माध्यम से `pendulum` को स्थापित किया है:

```shell
pip install pendulum
```

तब, वर्तमान दिनांक प्राप्त करने के लिए:

```python
import pendulum

today = pendulum.now().date()
print(today)  # परिणाम: YYYY-MM-DD (उदाहरण के लिए, 2023-04-05)
```

`Pendulum` के साथ, स्वरूपण भी सरल और `strftime` दृष्टिकोण के समान है:

```python
import pendulum

today = pendulum.now()
formatted_date = today.to_formatted_date_string()  # डिफ़ॉल्ट प्रारूप: "Apr 5, 2023"
print(formatted_date)
```

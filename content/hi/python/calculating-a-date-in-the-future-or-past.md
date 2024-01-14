---
title:                "Python: भावित में या भूत में एक तारीख की गणना"
simple_title:         "भावित में या भूत में एक तारीख की गणना"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# क्यों
किसी भी उचित तारीख को भविष्य या भूतकाल में गणना करने के लिए *क्यों* कोई भी काम किया जाएगा।

# कैसे करें
कोडिंग उदाहरण और "बैकटिक" प्राउटपुट के साथ "```पायथन ... ```" कोड ब्लॉक में।

उदाहरण:
```
# भविष्य की तारीख की गणना करें
from datetime import date, timedelta
today = date.today()
future_date = today + timedelta(days=7)
print("आज की तारीख:", today)
print("सप्ताह में 7 दिन बाद की तारीख:", future_date)
```

प्राउटपुट:
```
आज की तारीख: 2021-05-07
सप्ताह में 7 दिन बाद की तारीख: 2021-05-14
```

# गहराई में झाँकना
भविष्य या भूतकाल में तारीख की गणना करना जरूरी है क्योंकि यह आपको अपने काम को नियोजित करने में मदद करता है। यह आपको राजनीतिक, शैक्षणिक, लॉजिस्टिक और अन्य क्षेत्रों में प्राथमिकताओं को अनुमति देता है। इसके अलावा, आप एक स्पष्टता के साथ अपने काम को संगठित और अधिक पहुंचने वाला जरूरी हो।

# देखें भी
- [पायथन डेटा और समय मॉड्यूल की गाइड](https://docs.python.org/3/library/datetime.html)
- [पायथन में तारीख और समय की गणना करने के लिए गुरुत्वाकर्षण](https://github.com/agyorev/Gravity/blob/master/Gravity.py)
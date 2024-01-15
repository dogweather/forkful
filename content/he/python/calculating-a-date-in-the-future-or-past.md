---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Python: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# למה

במבט ראשון, חישוב תאריך בעתיד או בעבר יכול להראות כמו משימה מייגעת וחסרת טעם. אבל למתכנתים ומפתחי תוכנה, היכולת לחשב תאריך יכולה להיות כלי מאוד שימושי ופרקטי. זה יכול לסייע באוטומציה של משימות אישיות או עבודתיות ואפילו לחסוך זמן ומאמץ.

# כיצד לעשות זאת

```Python
# ייבא את המודול datetime
import datetime

# חשב את התאריך של 14 ימים מהיום
future_date = datetime.date.today() + datetime.timedelta(days=14)

# הדפס את התאריך החדש בפורמט תאריך מלא
print(future_date.strftime("%d/%m/%Y"))

# חשב את התאריך של 30 ימים אחורה מהיום
past_date = datetime.date.today() - datetime.timedelta(days=30)

# הדפס את התאריך החדש בפורמט יום בשבוע, חודש, יום
print(past_date.strftime("%A, %B %d"))
```

*פלט:*

```
28/11/2021
Thursday, October 28
```

# Deep Dive

החישוב של תאריך בעתיד או בעבר מבוסס על תאריך היום והוספת או הורדת ימים ממנו באמצעות timedelta טיפוס שמאפשר לנו ליצור תאריכים חדשים עם יחידות זמן שונות. כאשר נוסיף ימים לתאריך, התאריך החדש יהיה בעתיד, וכאשר נחסיר ימים, התאריך החדש יהיה בעבר. בנוסף, אנו יכולים להשתמש בפורמט כדי להציג את התאריך בצורה שונה, כגון יום בשבוע, חודש ותאריך מלא. זה יכול להיות שימושי כאשר אנו צריכים להציג את התאריך בפורמט שונה למשתמש או למטרות מסוימות.

## ראו גם

- [תיעוד רשמי על המודול datetime של פייתון](https://docs.python.org/3/library/datetime.html)
- [מדריך למודול datetime](https://realpython.com/python-datetime/)
- [דוגמאות לחישוב תאר
---
title:    "Python: חישוב תאריך בעתיד או בעבר"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

נתחשק לנו לחשב תאריך בעתיד או בעבר, יכול להיות כי חייבנו להכיר לאן יפול יום הולדת של חבר, או שאולי רוצים לחשב כמה זמן עבר מאז הטיול האחרון. בכל מקרה, בתכנות בפייתון ישנם כמה כלים מועילים לחישוב תאריכים בעתיד או בעבר.

## איך לעשות זאת

בשְׂפַת התכנות פייתון, יש פקודות מובנות לחישוב תאריכים בעתיד או בעבר. ראשית, נצטרך לייבא את הספרייה "datetime" כדי להשתמש בכלי החישוב הזה. אם נרצה לחשב תאריך מסוים בעתיד, נוכל להשתמש בפקודה "date.fromordinal()", עם הפרמטר של כמה ימים בעתיד מתאריך היום.

```Python
import datetime

future_date = datetime.date.fromordinal(739021)
print(future_date)
```

פלט:

```
2021-05-15
```

ניתן גם להשתמש בפקודה "date.today()" כדי לקבוע את התאריך הנוכחי ולהוסיף לו כמה ימים על ידי שימוש בפקודה "timedelta()".

```Python
import datetime

today = datetime.date.today()
days_to_add = datetime.timedelta(days=30)
future_date = today + days_to_add
print(future_date)
```

פלט:

```
2021-06-09
```

לחישוב תאריך בעבר, נוכל להשתמש בפקודה "date.fromtimestamp()", עם הפרמטר של כמה שניות לפני תאריך היום.

```Python
import datetime

past_date = datetime.date.fromtimestamp(1614412800)
print(past_date)
```

פלט:

```
2021-02-27
```

## העמקה

בפייתון קיימות גם פקודות נוספות לחישוב תאריכים בעתיד או בעבר, כגון "date.fromisoformat()" ו-"date.fromisoformat()". כמו כן, ניתן להשתמש בפקודות להצגת התאריך בפורמטים שונים או לחישוב הפרש זמן בין שני תאריכים.

בכל זאת, חשוב לזכור שתאריך בפייתון מוגדר כאובייקט ולכן
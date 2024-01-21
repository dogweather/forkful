---
title:                "חישוב תאריך בעתיד או בעבר"
date:                  2024-01-20T17:31:34.791985-07:00
model:                 gpt-4-1106-preview
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריכים בעתיד או בעבר הוא תהליך שבו אנחנו מחשבים תאריכים שיקרו אחרי או לפני נקודת זמן מסוימת. תכנתנים מבצעים זאת לצרכי תזמון אירועים, תזכורות, וניהול נתונים בזמן אמיתי.

## איך לעשות:
אם אתם רוצים לבצע פעולות על תאריכים בשפת Gleam, תצטרכו ספריית צד שלישי, כיוון שגרסה זו אינה כוללת מודול זמן מובנה. אנחנו נשתמש בספרייה `gleam_datetime` (זה דוגמה, לא שם אמיתי של ספרייה, שכן לא קיימת כרגע ספרייה רשמית לתאריכים בGleam).

```gleam
import gleam_datetime

pub fn add_days_to_date() {
  let today = gleam_datetime.local_date(2023, 4, 1)
  let future = gleam_datetime.add_days(today, 30)
  gleam_datetime.to_string(future)
}
```

זהו. פלט הפונקציה יהיה תאריך 30 ימים לאחר ה-1 באפריל, 2023, כלומר, ה-1 במאי, 2023.

## עיון מעמיק
חישובי תאריכים הם חלק חשוב בתכנות מאז שהמחשבים הראשונים הופיעו. בשפות תכנות שונות ישנם מודולים לטיפול בזמן ובתאריכים. בג'אווה יש `java.util.Date`, בפייתון יש `datetime`, וכן הלאה. בGleam, עדיין צריך להמתין לספרייה רשמית או להשתמש בספריות צד שלישי. עם התרחבות השפה, יתכן והיא תכלול מנגנון כזה מובנה.

פרט לזאת, תמיד יש לקחת בחשבון בעיות כמו קיץ שעון, שינויי לוח שנה, וההפרשים בין אזורי זמן שונים. תכנות הוא לא רק לדעת את הפקודות אלא גם להבין את ההקשר שבו הן פועלות.

## ראה גם
- ["Awesome Gleam"](https://github.com/gleam-lang/awesome-gleam) - מאגר GitHub עם רשימת משאבים וספריות שימושיות לGleam.
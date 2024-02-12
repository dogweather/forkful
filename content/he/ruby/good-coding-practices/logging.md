---
title:                "רישום פעולות (לוגים)"
date:                  2024-01-26T01:09:42.969575-07:00
model:                 gpt-4-1106-preview
simple_title:         "רישום פעולות (לוגים)"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/logging.md"
---

{{< edit_this_page >}}

## מה ולמה?
רישום (לוגים) בתיכנות הוא כמו להחזיק יומן עבור האפליקציה שלך. זהו תיעוד מסודר של אירועים, הודעות ונקודות נתונים הנותנים לך תובנה לגבי מה שהאפליקציה שלך עושה ואיך היא מתנהגת. מתכנתים מבצעים רישום כי זה קריטי לאיתור באגים, מעקב אחר בריאות האפליקציה, וקבלת רמזים אודות בעיות פוטנציאליות לפני שהן הופכות לבעיות אמיתיות.

## איך לעשות:
רובי כוללת מודול מובנה לרישום, `Logger`, שהשימוש בו קל מאוד. הנה דוגמה מהירה להתחלה:

```ruby
require 'logger'

# יצירת Logger שמוציא מידע ל-STDOUT
logger = Logger.new(STDOUT)
logger.level = Logger::INFO

# הודעות לדוגמה ליומן
logger.info("This is an info message")
logger.warn("This is a warning message")
logger.error("This is an error message")
```

הרצת הסקריפט לעיל תפלט משהו כזה:

```
I, [2023-03-15T10:00:00.123456 #1234]  INFO -- : This is an info message
W, [2023-03-15T10:00:01.234567 #1234]  WARN -- : This is a warning message
E, [2023-03-15T10:00:02.345678 #1234] ERROR -- : This is an error message
```

ניתן להגדיר את פורמט היומן ואת רמתו כדי לסנן רעשים לא נחוצים, ואפשר להפנות יומנים לפלטים שונים, כמו קובץ או אפילו שירות רישום חיצוני.

## צלילה עמוקה
רישום הוא כמו מסורת עתיקה בתיכנות. בעבר, היומנים היו קבצי טקסט פשוטים, שנפרשו באופן ידני באמצעות כלים כמו `grep`. אבל המושג התפתח לכדי מערכת שלמה של פריימוורקים ושירותי רישום מתקדמים כמו Log4j, Syslog בלינוקס, או Sematext ו-Loggly בעידן הענן.

`Logger` של רובי הוא דרך ללא פריטים מיותרים להתחיל, אבל אם אתה זקוק ליותר כוח פרדס וגמישות, כדאי לבדוק אלטרנטיבות כמו Lograge או Semantic Logger. הספריות הללו עובדות טוב עם אפליקציות רובי, ומציעות שליטה יותר דקדקנית על פורמט הרישום, כולל יומנים מובנים (בפורמט JSON), ביצועים טובים יותר, ואינטגרציה חלקה עם שירותים אחרים.

לכל ספריית רישום של רובי יש את הדרך שלה לעשות דברים, אבל בסופו של דבר כולן מבוססות על הרעיון של מופע logger שאליו שולחים הודעות. ה-logger מטפל בהודעות אלו בהתבסס על רמות מוגדרות—DEBUG, INFO, WARN, ERROR, FATAL, ו-UNKNOWN—ומחליט מה לעשות איתן: להדפיס אותן, לשמור אותן בקובץ, לשלוח אותן דרך הרשת, וכו'.

## ראה גם
לצלילה עמוקה יותר אל תוך מודול הרישום המובנה של רובי, עיין במסמכים הרשמיים:

אם אתה מתעניין ברישום מתקדם יותר או רוצה לחקור גמזים של צד שלישי:
- [Lograge](https://github.com/roidrage/lograge)

לקריאה כללית על מתודולוגיות ופילוסופיה של רישום (לא ספציפית לרובי), מאמרים אלו הם קריאה חובה:
- [ספר הנדסת אמינות אתרים של גוגל - פרק 16: טיפול בעומס](https://sre.google/sre-book/handling-overload/#log-messages)
- [האפליקציה ב-12 פקטורים - יומנים](https://12factor.net/logs)
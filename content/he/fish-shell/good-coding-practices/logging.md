---
date: 2024-01-26 01:08:35.329213-07:00
description: "\u05DC\u05D5\u05D2\u05D9\u05DD \u05D4\u05DD \u05D1\u05E2\u05E6\u05DD\
  \ \u05E8\u05D9\u05E9\u05D5\u05DD \u05E9\u05DC \u05DE\u05D4 \u05E9\u05D4\u05D9\u05D9\
  \u05E9\u05D5\u05DD \u05E9\u05DC\u05DA \u05E2\u05D5\u05E9\u05D4 - \u05D9\u05D5\u05DE\
  \u05DF, \u05D0\u05DD \u05EA\u05E8\u05E6\u05D5, \u05D0\u05D1\u05DC \u05E2\u05D1\u05D5\
  \u05E8 \u05E7\u05D5\u05D3. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E2\u05E7\u05D5\
  \u05D1 \u05D0\u05D7\u05E8\u05D9 \u05D4\u05E4\u05E8\u05D8\u05D9\u05DD \u05D4\u05E7\
  \u05D8\u05E0\u05D9\u05DD, \u05DB\u05DE\u05D5 \u05E9\u05D9\u05E0\u05D5\u05D9\u05D9\
  \ \u05DE\u05E6\u05D1, \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9 \u05DE\u05E2\u05E8\u05DB\
  \u05EA,\u2026"
lastmod: '2024-03-13T22:44:40.060434-06:00'
model: gpt-4-1106-preview
summary: "\u05DC\u05D5\u05D2\u05D9\u05DD \u05D4\u05DD \u05D1\u05E2\u05E6\u05DD \u05E8\
  \u05D9\u05E9\u05D5\u05DD \u05E9\u05DC \u05DE\u05D4 \u05E9\u05D4\u05D9\u05D9\u05E9\
  \u05D5\u05DD \u05E9\u05DC\u05DA \u05E2\u05D5\u05E9\u05D4 - \u05D9\u05D5\u05DE\u05DF\
  , \u05D0\u05DD \u05EA\u05E8\u05E6\u05D5, \u05D0\u05D1\u05DC \u05E2\u05D1\u05D5\u05E8\
  \ \u05E7\u05D5\u05D3. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E2\u05E7\u05D5\u05D1\
  \ \u05D0\u05D7\u05E8\u05D9 \u05D4\u05E4\u05E8\u05D8\u05D9\u05DD \u05D4\u05E7\u05D8\
  \u05E0\u05D9\u05DD, \u05DB\u05DE\u05D5 \u05E9\u05D9\u05E0\u05D5\u05D9\u05D9 \u05DE\
  \u05E6\u05D1, \u05D0\u05D9\u05E8\u05D5\u05E2\u05D9 \u05DE\u05E2\u05E8\u05DB\u05EA\
  ,\u2026"
title: "\u05E8\u05D9\u05E9\u05D5\u05DD \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA (\u05DC\
  \u05D5\u05D2\u05D9\u05DD)"
---

{{< edit_this_page >}}

## מה ולמה?
לוגים הם בעצם רישום של מה שהיישום שלך עושה - יומן, אם תרצו, אבל עבור קוד. מתכנתים עושים זאת כדי לעקוב אחרי הפרטים הקטנים, כמו שינויי מצב, אירועי מערכת, ותקלות מעצבנות, ולוודא שאף תקלה לא חומקת ללא שימת לב.

## איך לעשות:
ב-Fish, לוגים יכולים להיות פשוטים כמו הכוונת זרמי פלט ושגיאה סטנדרטיים לקובץ. בואו ניצור רישום לוג לזמני ההתחלה והסיום של הסקריפט שלנו.

```fish
function log_start
  echo (date "+%Y-%m-%d %H:%M:%S") " - הסקריפט התחיל" >> my_app.log
end

function log_end
  echo (date "+%Y-%m-%d %H:%M:%S") " - הסקריפט הסתיים" >> my_app.log
end

log_start
# ... משימות הסקריפט שלך ...
log_end

cat my_app.log
```

הנה מה שתראו ב-`my_app.log`:

```
2023-04-01 10:35:47  - הסקריפט התחיל
2023-04-01 10:36:02  - הסקריפט הסתיים
```

ללוגים מתקדמים, אתם יכולים להשתמש בפונקציות עם פרמטרים לרמת הלוג וההודעות:

```fish
function log_message --argument message
  switch "$argv[1]"
    case 'INFO' 'WARN' 'ERROR'
      set log_level $argv[1]
    case '*'
      set log_level 'DEBUG'
  end
  set log_msg (string join " " $argv[2..-1])
  echo (date "+%Y-%m-%d %H:%M:%S") "[$log_level]" $log_msg >> my_app.log
end

log_message INFO "זו הודעה מידעית."
log_message ERROR "משהו השתבש!"
```

דוגמת פלט `my_app.log` תהיה:
```
2023-04-01 10:35:47 [INFO] זו הודעה מידעית.
2023-04-01 10:35:49 [ERROR] משהו השתבש!
```

## צלילה עמוקה
בעבר, לוגים בסקריפטי של עבודת מעטפת נעשו עם שורת פקודות `echo`, ולמרות שזה בהחלט עדיין אפשרות, יישום של מערכות מורכבות יותר יכול להוות אתגר. ל-Fish אין מנגנון לוגים מובנה כמו לשלל מעטפות או שפות תכנות אחרות, כך שלעתים קרובות יש ליצור אחד משלך.

חלופות לפקודת `echo` המובנית של Fish לצורך לוגים כוללות כלים מערכתיים של Unix כמו `syslog` או `logger`, שמתממשקים עם שירות הלוג המערכתי, ומספקים גישה מובנית יותר לרישום אירועים ברמת המערכת שבוצעו.

הפשטות של Fish מאפשרת לך ליצור פונקציות לטיפול בפרטניות של הלוגים, ולהגדיר רמות שונות שאתה יכול להפעיל או לכבות. יישומים מסוימים אפילו יכולים לכלול את שם הסקריפט, מספר השורה וחותמת זמן, מה שמקל על החזרה בצעדים שהובילו לאירוע.

## ראה גם
- התיעוד של Fish Shell על כתיבת פונקציות: https://fishshell.com/docs/current/#syntax-function
- טיפים בסיסיים לתכנות סקריפטים בשל: https://developer.ibm.com/tutorials/l-lpic1-103-4/
- מדריך לפרוטוקול Syslog: https://tools.ietf.org/html/rfc5424

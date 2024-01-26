---
title:                "רישום פעולות (לוגים)"
date:                  2024-01-26T01:08:35.329213-07:00
model:                 gpt-4-1106-preview
simple_title:         "רישום פעולות (לוגים)"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/logging.md"
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
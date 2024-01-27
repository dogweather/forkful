---
title:                "רישום פעולות (לוגים)"
date:                  2024-01-26T01:09:11.378900-07:00
model:                 gpt-4-1106-preview
simple_title:         "רישום פעולות (לוגים)"
programming_language: "Python"
category:             "Python"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/logging.md"
---

{{< edit_this_page >}}

## מה ולמה?
רישום (Logging) הוא תהליך של תיעוד אירועים ביישום במהלך פעילותו, והוא מספק עקבות לניתוח לאחר מותו של התוכנית ולניטור בזמן אמת. מתכנתים עושים זאת מכיוון שזה עוזר לאבחן בעיות, לנטר ביצועים, ולעקוב אחר פעולות המשתמש מטעמי אבטחה וניתוח.

## איך לעשות:
בפייתון יש מודול מובנה לרישום. הנה הגדרה בסיסית:
```Python
import logging

# תצורה בסיסית של רישום
logging.basicConfig(level=logging.INFO)

# שליחת הודעות לרישום
logging.debug('This is a debug message')
logging.info('מידע על מה שהתוכנית שלך עשתה כרגע')
logging.warning('הודעת אזהרה')
logging.error('אירעה שגיאה')
logging.critical('התוכנית לא מסוגלת להתאושש!')
```
כאשר תריץ את הקוד הזה, תראה את הפלט הבא (מכיוון שרמת הברירת המחדל היא WARNING, ההודעות debug ו-info לא יוצגו):
```
WARNING:root:הודעת אזהרה
ERROR:root:אירעה שגיאה
CRITICAL:root:התוכנית לא מסוגלת להתאושש!
```
ניתן גם להגדיר את הרישום לכתיבה לקובץ במקום לקונסולה:
```Python
logging.basicConfig(filename='app.log', filemode='w', level=logging.INFO)
```
עכשיו הרישומים שלך יועברו לקובץ 'app.log'.

## צלילה עמוקה
רישום קיים מהימים הראשונים של התכנות, כאשר יומני המערכת היו אחת מצורות האחסון העקביות הוותיקות ביותר מחוץ לקבצים אשר החזיקו בנתונים בפועל. מלבד ההיסטוריה, הרעיון המרכזי של רישום נשאר למעשה ללא שינוי, אף על פי שהכלים התפתחו.

מודול ה`logging` בפייתון הוא חזק וגמיש מאוד. הוא מאפשר למתכנתים להגדיר רמות רישום שונות (DEBUG, INFO, WARNING, ERROR, CRITICAL) שיכולות לעזור בקטגוריות וסינון של רישומים. יש לו מערכת לוגרים היררכית, כלומר ניתן ליצור קשרי הורים-ילדים בין לוגרים ולהפיץ הודעות לאורך השרשרת.

אלטרנטיבות כוללות ספריות של צד שלישי כמו Loguru או structlog שמציעות תכונות מתקדמות וממשק פשוט יותר מאשר מודול הרישום המובנה. הן יכולות להציע פלט יפה יותר, סריאליזציה טובה יותר של נתונים מבניים, ודרכים יותר אינטואיטיביות להתמודד עם תצורת לוג.

בנוגע ליישום, כאשר מגדירים רישום חשוב לעשות זאת פעם אחת בתחילת היישום. מומלץ לקבוע את התצורה ברמת המודול באמצעות `logging.getLogger(__name__)` כדי לעקוב אחר מיטב התרגולים של רישום פייתון.

רישום לא אמור להשפיע משמעותית על ביצועי היישום בנסיבות רגילות. עם זאת, יש להיזהר במה שנרשם: רישום מרובה מדי, במיוחד ברמות DEBUG, יכול להאט את היישום ולמלא מהר את אחסון קבצי הלוג.

## ראה גם
למידע נוסף על מודול הרישום של פייתון, בדוק את ספר הבישול הרשמי של רישום פייתון עבור דוגמאות נהדרות ומיטב התרגולים: https://docs.python.org/3/howto/logging-cookbook.html

להצצה מעמיקה יותר בנושא רישום מבני וכיצד הוא יכול לסייע להפוך את הלוגים למודיעיניים וקלים יותר לניתוח, Loguru מתועד היטב: https://loguru.readthedocs.io

כמו כן, שקול להסתכל על מתודולוגיית האפליקציה ב-12 הגורמים, בפרט על הסעיף העוסק בלוגים לנקודת מבט מודרנית על רישום ביישומים: https://12factor.net/logs
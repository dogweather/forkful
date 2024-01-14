---
title:                "Fish Shell: עיבוד html"
simple_title:         "עיבוד html"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## מדוע

פירוש הוא התכנות ב־Fish Shell שואב את התוכן של דף HTML ומפענח אותו לצורה שניתן לעבוד איתה בקלות יותר. זה יכול להיות שימושי כאשר יש לנו צורך לעבוד עם נתונים מרוכזים מהאיזור הפנימי של אתר אינטרנט.

## איך לעשות זאת

תחום ה־web-scraping נהיה קצת מסובך כאשר אנחנו צריכים לעבוד עם דפי HTML מורכבים. למרבה המזל כי Fish Shell מגיע עם כמה הצעות לפתרונות שימושיים.

הנה דוגמאות קוד המראות כיצד להשתמש בכלי ש־Fish Shell מציע כדי לפענח דפי HTML:

```fish
curl -s <url> | pup <selector>
```

פקודה זו תפענח את הדף ב־URL שנשלח כפרמטר ותבחר את אלמנטי ה־HTML המתאימים ל־<selector> שנשלח. למשל, אם נשלח `<title>`, הכלי יחזיר את השם של האתר מתוך ה־<title> tag.

אם נרצה לקבל את נתוני האתר בצורה ארגומנטים של Fish Shell, נוכל להשתמש ב־`dom` ו־`content`:

```fish
set response (curl -s <url> | pup <selector>)
if set -l title (string split -m1 \t (string trim \t $response))
  echo (string trim -c - $title[2])
end
```

כאן אנחנו משתמשים בתוצאה של הכלי כתשובה, ונבדוק עבור `<title>` tag. אם נמצא כותרת, נבצע פעולת echo על התוכן מתוך ה־<title> tag.

## חפירה עמוקה

Fish Shell מציע מספר כלים מובנים נוספים לפענח דפי HTML. אפשר גם להשתמש ב־XPath כדי לבחור את האלמנטים הרצויים. יש גם אפשרויות לתיקון מבנה של דפי HTML על ידי הוספת tag נסתרים ומרחיבים.

בנוסף, ישנם כלים חיצוניים שניתן להשתמש כדי להרחיב את יכולות ה־web
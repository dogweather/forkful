---
title:                "הגדלת אותיות במחרוזת"
html_title:           "Ruby: הגדלת אותיות במחרוזת"
simple_title:         "הגדלת אותיות במחרוזת"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
הפונקציה של אות גדולה במחרוזת (string) היא אין סיום. לראשית, הוא מאפשר לנו להבליט מילים מסוימות. לדוג' בשפות תכנות אחרות, אולי תרצה להראות מילים קטלניות כמו ERRORS או WARNING גדול. זה לא רק עניין של עיצוב, אלא יכול גם לעזור לקורא למצוא את המידע החשוב באופן מהיר.

## נסיון:
בעזרת Ruby, אפשר להעלים את כל מחרוזת באמצעות השיטה `upcase`. הנה איך זה נראה:

```Ruby
str = "hello there, programmers!"
puts str.upcase

# התוצאה תהיה: "HELLO THERE, PROGRAMMERS!"
```
או, אם ברצוננו להפוך רק את האות הראשונה של כל מילה לאותיות גדולות, אנו יכולים להשתמש בפעולה `capitalize` או `titleize`: 

```Ruby
str = "hello there, programmers!"
puts str.capitalize

# התוצאה תהיה "Hello there, programmers!"
```
## ברוך טבילה:
הוא נפוצה מספיק בשפות תכנות שיש לו פונקציונאליות משלו ברוב המקרים, ובכל המקרה הוא תמיד מאוד פשוט להשג אם אתה מכיר מעט מדי סינטקס של השפה. ישנם גם דרכים נוספות להגיע לאותו התוצאה, למשל, בעזרת תכנות מערך או באמצעות שימוש בספריות מקודדות.

הזהרת והתחפושת הכי טובה ברוב המקרים תהיה להשתמש בפונקציה כדי להפוך את המחרוזת לאותיות גדולות, אבל בשעה האחרונה אתה יכול למצוא את הכוח של הגדרת משתנה חדש של מידע המקיים את המחרוזת המקורית.

## ראה גם:
[מדריך תיעוד של Ruby](https://www.rubydoc.info/stdlib/core/String:capitalize) - ראה איך `capitalize` `upcase` ו `titleize` עובדים. וכן [מדריך עזרה](https://www.tutorialspoint.com/ruby/ruby_strings.htm) ולראות איך להשתמש באילו עזרים.
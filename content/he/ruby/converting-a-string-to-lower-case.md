---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Ruby: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה

ישנם מספר סיבות למה אדם יבחר להמיר מחרוזת לאותיות קטנות בשפת Ruby. כמה מהסיבות העיקריות הן:

- להקל על קריאה והשוואה של מחרוזות, כיוון שאותיות קטנות וגדולות מחשבות במחרוזת זהה יכולות להיות מבעייתיות.
- לטיפול במידע מקורי, בדרך כלל תוך המרה ללא תלות בפורמט של המחרוזת המקורית, כך שניתן להיעזר בתוכנית באופן גמיש יותר.

## איך לעשות זאת

תהליך ההמרה לאותיות קטנות בשפת Ruby יכול להיות פשוט כמו שניתן לראות בקטע הקוד הבא:

```Ruby
my_string = "HELLO WORLD"
lowercase_string = my_string.downcase
puts lowercase_string
```
תוצאה: "hello world"

ניתן גם להשתמש בפונקציה של חברת Ruby כדי לבצע את ההמרה במקום:

```Ruby
my_string = "HELLO WORLD"
lowercase_string = my_string.mb_chars.downcase
puts lowercase_string
```
תוצאה: "hello world"

## העומק של ההמרה לאותיות קטנות

בכדי להבין טוב יותר את התהליך של המרת מחרוזת לאותיות קטנות בשפת Ruby, ניתן להתחיל בהבנת מה קורה מאחורי הקלט של הפונקציה. בגירסאות קודמות של Ruby, הייתה פונקציה נפרדת בשם `String.downcase` שהייתה אחראית למרות מחרוזת לאותיות קטנות. אך בגרסת המרה הנוכחית, הפונקציה יכולה להתאים את עצמה לנתונים שהיא מקבלת. לדוגמה, אם המחרוזת כוללת אותיות גדולות בלבד, הפונקציה תחזיר מחרוזת זהה בלי שינויים.

## ראו גם

- [מסמכי רמת Ruby הרשמיים](https://www.ruby-lang.org/en/documentation/)
- [תיעוד על המרות מחרוזת ב-Ruby](https://ruby-doc.org/core-2.7.2/String.html#method-i-downcase)
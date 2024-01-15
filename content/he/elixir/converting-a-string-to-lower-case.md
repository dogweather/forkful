---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Elixir: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה

במאמר זה נדבר על המתודה `String.downcase/1` בשפת אליקסיר ועל התועלת של המרה של מחרוזת לאותיות קטנות בשפת בינונית. נבין למה זה יכול להיות שימושי ונראה כיצד להשתמש בו בצורה נכונה.

## איך לעשות את זה

בשפת אליקסיר, יש לנו את המתודה `String.downcase/1` שמקבלת מחרוזת ומחזירה אותה באותיות קטנות. ניתן להתאים אותה כך:

```Elixir
String.downcase("HELLO") # output: "hello"
```

אם המחרוזת מכילה כבר אותיות קטנות, המתודה תחזיר אותה באותו צורה.

```Elixir
String.downcase("Hello world") # output: "hello world"
```

ניתן להשתמש במארחת אלתורים כדי לבדוק את התוצאות של המתודה עם כמה נתונים משתנים כדי לוודא שהוא עובד כצפוי.

```Elixir
String.downcase("HeLlO") # output: "hello"
String.downcase("123") # output: "123"
String.downcase("HeLlO123") # output: "hello123"
```

## צלילה עמוקה

מתודה `String.downcase/1` היא חלק מכמה מתודות אחרות במודול `String` המבוססת על תהליך אליפבי. כל אותיות מיוצגות על ידי מספרי יסוד ולכן אפשר להשתמש במתודה גם עם תווים מחרוזות שאינם אותיות במקרה הצורך.

אם אתם צריכים לבצע פעולות מתמטיות על מחרוזת, כמו לחשב את אורך המחרוזת או למצוא את המילה הראשונה במחרוזת, כדאי להשתמש במתודה `String.downcase/1` תחילה כדי לוודא שכל האותיות נמצאות באותו המספר של היסוד.

## ראה גם

- [מאמר על מתודת `String.upcase/1` באליקסיר](https://example.com/upcase)
- [מדריך רשמי במסמכי לו"ז אליקסיר למתחילים](https://example.com/elixir-docs)
---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Gleam: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# למה

במאמר זה אנו נלמד כיצד להמיר סטרינג לכתב קטן בשפת גלים (Gleam). הכתב הקטן מתייחס לאותיות בעלות תבנית נמוכה, כמו אותיות קטנות או מספרים.

בעזרת הפעולה הזו, נוכל להשתמש בסטרינגים בצורה יותר מגוונת ומדוייקת, כך שהמידע שאנו מעבירים יהיה תואם לציפיות ולפרטים שמציגים.

# איך לעשות זאת

לפניכם שתי דוגמאות להמרת סטרינג לכתב קטן בשפת גלים. ניתן להשתמש בבלוקי קוד "```Gleam ...```" כדי להראות את התוצאות של הקוד.

```Gleam
str = "Hello World"
lowercase = String.to_lower_case(str)
```

כתוצאה מהקוד הנ"ל, המשתנה "lowercase" יכיל את הסטרינג "hello world" בכתב קטן.

```Gleam
num = "123ABC"
lowercase_num = String.to_lower_case(num)
```

במקרה זה, הסטרינג "123ABC" יהפוך למספרים ואותיות קטנות - "123abc".

# חקירה מעמיקה

הפעולה "String.to_lower_case" משתמשת באלגוריתם להמרת סטרינג לאינדקס שמתאים לכתב קטן בלבד. כלומר, התוצאה הסופית תתאים בדיוק למה שמציגים במונח "כתב קטן". עקב כך, הפעולה זו יכולה להיות מועילה למשתמשי תוכניות שונים, כמו מניפולציה במילים או באינדקסים.

# ראו גם

* תיעוד שפת גלים: https://gleam.run/documentation
* דוגמאות של שימושים במתודות של מחרוזות (Strings): https://github.com/gleam-lang/gleam/blob/master/stdlib/string/string.gleam
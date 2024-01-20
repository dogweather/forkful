---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

היי שם, בואו נדבק ישר בעסיס.

## מה ולמה?
חיפוש א length של מחרוזת הוא בעצם ספירת התווים שיש בתוך מחרוזת כלשהי. אנחנו, תכנתים, צריכים את זה בתכנות לכמה סיבות, למשל כדי לראות אם נתון מסוים עונה על דרישות קלט, או בעת מהלך אלגוריתמים אותם אנו יוצרים.

## איך ל:
על גבי Ruby, אתה פשוט משתמש באמצעי ה `.length` במחרוזת. 

```Ruby
s = "Hello World"
puts s.length
```

כאשר אתה מריץ את הקוד שלמעלה, אתה תראה שהפלט הוא `11`. זהו מספר התווים במחרודת "Hello World".

## Deep Dive
מאז המזח של ruby בשנת 1995, מקרי השימוש של `.length`, `.size` ואף `.count` היו נמשך. עם זאת עבור מחרוזות, כל שלושת השיטות מחזירות את מספר התווים. 

אולם שימו לב, בעת שילוב עצמים מתוך מערכים או בני מאפיינים מסוימים, התוצאות של `.length` ו `.count` עלולות להיות שונות.

```Ruby
s = "Hello World"
puts s.chars.length
```

הקוד שלמעלה ידפיס "11" כי `.chars` ישנה את המחרוזת למערך של תווים ואז `.length` מחזירה את כמות הפריטים במערך.

## ראה גם
אתם תמיד יכולים לקרוא עוד בנושא זה [בתיעוד הרשמי של Ruby](https://ruby-doc.org/core-2.7.0/String.html#method-i-length) או [במדריכים נוספים](https://devdocs.io/ruby~2.7/string#method-i-length) שאינטרנט מציעה.
---
title:                "שימוש בביטויים רגולריים"
html_title:           "Gleam: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## למה
ביטויים רגולריים הם כלי עוצמתי בתכנות שימושי עבור מתכנתים שרוצים לחפש ולהתאים טקסטים בדיוק או להחליף אותם בצורה כללית.

## איך להשתמש
תחילה, צריך ליידע את עצמך עם טקסט רגולרי של גלים, המתמזג בין הכרך הרבה הטקסט עמודים ולמה בואיס תצורה את יכולת ייצוג. קדימ: לייצור את אורח החיפוש. את הדוגרמא ניתן לראות את השאילתא הבאהעמוד אותרך כל עדכון כי גם הסטטוס הסוכם.

\`\`\`Gleam
let regex = regex"\bHello, World!\b"

let matches = regex.match("Hello, World!")
\`\`\`

הנה התוצאה של תוכנית זו:

\`\`\`
matches = Ok(["Hello, World!"])
\`\`\`

עם שימוש בטקסט רגולריים, אפשר גם להחליף טקסט בצורה כללית. נקודת המוצא יכולה להיות מיכל אותיות, ותוכל הנוכחיות של כל נתיב תוכל להחליף אותם לטקסט אחר. הנה דוגמא לכך:

\`\`\`Gleam
let regex = regex"\bGleam\b"

let replace = regex.replace("This is Gleam programming language!", "Rust")

// שליחה מפורטוגל
replace = Ok("This is Rust programming language!")
\`\`\`

## חפירה עמוקה
בנוסף לביצוע בסיסי של חיפוש והחלפה, טקסט רגולריים מאפשרים גם חיפוש מתקדם עם תבניות ופעולות שונות. תבניות יכולות להיות זה ממילון, מספרים, אינדקסים ועוד. פעולות נוספות כוללות את הסתגלות משתנים, החפוש לפני או אחרי מילה ספציפית ועוד.

## ראה גם
- [הדרך המצוינת של גלים של פיתון לביטויים רגולריים](https://www.theexved.com/blog/python/regular-expressions-with-gleam/)
- [ייש
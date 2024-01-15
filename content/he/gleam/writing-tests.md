---
title:                "כתיבת מבחנים"
html_title:           "Gleam: כתיבת מבחנים"
simple_title:         "כתיבת מבחנים"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## למה

בניסוח פשוט, בלוגר, מתכנת או מפתח ימצא עצמם תמיד מתמודדים עם בעיות ותקלות בקוד שלהם. כתיבת בדיקות מאפשרת לנו לזהות תקלות ובעיות שעלולות להיגרם מקוד לפני שהן מבחינת בקוד הראשי שלנו.

## איך לעשות זאת

```Gleam
post "הספריה hash בונה דו צדדי {
import hash

test "post לא מקבל את אותו הברירת מחדל עבור זוגות ערכים שונים" {
  assert_eq(hash.post({"a": 1}), hash.Paired(1, 2))
}

fn test_post {
  assert_eq(hash.post({"b": "abc"}), hash.Paired(1, "abc"))
}
}
```

כאן, אנחנו כותבים בדיקות לפונקציה post בתוך הספריה hash. ניתן לראות את הקלט והפלט המצופים עבור כל תרחיש.

## חפר עמוק

כאשר אנחנו כותבים בדיקות, אנחנו יכולים לבדוק לא רק את התוצאה המצפה, אלא גם מצבים קצה, כניסות שגויות ועוד. כתיבת בדיקות מקנה לנו ביטחון שהקוד שלנו כולל את כל הסיטואציות האפשריות ועובד כמתוכנן.

## ראו גם

- [כתיבת בדיקות עם גלים](https://www.gleam.run/learn/testing/)
- [מדריך לכתיבת בדיקות עם גלים](https://github.com/gleam-lang/gleam/blob/master/docs/guide/testing.md)
---
title:                "Clojure: חיפוש והחלפת טקסטים"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה

תחת כל פרוייקט תכנות ייתכנו מצבים שבהם עלינו לחלות רצף של טקסט ולהחליפו עם מחרוזת אחרת. לדוגמה, ייתכן שנוסיף או נחליף מילים במסמך או בקובץ קוד. השימוש במחרוזות יכול לעזור לנו להחליף את הטקסט בצורה יעילה ומהירה.

## איך לעשות זאת

לכל מי שמעוניין לחלות טקסט ולהחליפו במחרוזת אחרת בשפת Clojure, יהיה עליו להשתמש בפונקציית `replace`, המקבלת שני ארגומנטים - הראשון הוא הטקסט המקורי והשני הוא המחרוזת המדויקת. למשל:

```Clojure
(replace "שלום עולם!" "עולם" "אדון")
```

תוצאה: "שלום אדון!"

## טיול עמוק

ניתן לנסות גם להשתמש בפונקציות נוספות כמו `replace-first` ו- `replace-regexp` לשנים הנוספית ולחילופין של טקסט לפי תבנית מסוימת. בנוסף, ישנם כמה מודלים לשפת Clojure שמציעים פתרונות בכדי לבצע החלפת טקסט בצורה יעילה יותר.

## ראה גם

- [פונקצית replace](https://dzone.com/articles/clojure-string-function-replace)
- [מדריך מקיף על החלפת טקסט בשפת Clojure](https://www.baeldung.com/clojure-replace)
- [מידע על פונקציות נוספות כמו `replace-first` ו- `replace-regexp`](https://tech.noredink.com/post/151494568018/string-replace-in-clojure)
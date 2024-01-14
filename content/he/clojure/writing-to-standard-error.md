---
title:    "Clojure: כתיבה לשגיאת סטנדרט"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה

כתיבה לפלט שגיאות היא חלק חשוב מתהליך התכנות. היא מסייעת לנו למצוא ולתקן בעיות בקוד במהירות וביעילות רבה יותר. בכתיבה זו נראה כיצד ניתן להשתמש בשפת Clojure כדי לכתוב לפלט שגיאות בצורה נכונה ויעילה.

## איך לעשות זאת

לפני שנדבר על כיצד לכתוב לפלט שגיאות בשפת Clojure, נצטרך כמה כלים על מנת לבדוק את הקוד שלנו. האינסטרומנטות היא כלי שימושי שיעזור לנו למצוא באופן יעיל את השגיאות שלנו. הנה דוגמה של קוד בשפת Clojure עם שגיאת מגבלה בזמן ריצה שנכתבה לפלט השגיאה:

 ```Clojure
(defn my-function [x]
  (if (zero? x)
    (throw (Exception. "x cannot be zero"))
    (+ x 2)))

(my-function 0)
```

Output:

```
Exception in thread "main" java.lang.Exception: x cannot be zero
 at user/my-function (form-init1887379627502286802.clj:2)
 at user/eval20 (form-init1887379627502286802.clj:10)
 at user/run! (form-init1887379627502286802.clj:14)
 at user/run- (form-init1887379627502286802.clj:29)
 at user/run (form-init1887379627502286802.clj:31)
 at user/main (form-init1887379627502286802.clj:36)
 at user/main (form-init1887379627502286802.clj:36)
```

כאן אנו משתמשים בתנאי `if` כדי לבדוק אם המספר `x` הוא 0. אם זהו המקרה, אנו משתמשים בפונקציה `throw` כדי להפוך את הערך `Exception` למספר. הפונקציה `defn` מגדירה פונקציה חדשה בשם `my-function` שמקבלת פרמטר `x`. סופסות הפעולה של הפונקציה היא להוסיף 2 למספר `x` הנתון. כמו ברוב שפות התכנות, ניתן לשנות את הטקסט של השגיאה ע"י שימוש בפונקציית `format`, ולא יהיה דבר מפתיע אם נרצה להוסיף מספר שגיאות כאלה לפ
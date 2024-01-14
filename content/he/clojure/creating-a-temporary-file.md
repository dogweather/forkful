---
title:    "Clojure: יצירת קובץ זמני"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## למה?

הצורך ביצירת קובץ זמני יכול להיות טכני או פרקטי. לדוגמה, ייתכן שתרצו ליצור קובץ זמני בכדי לייצג נתונים מסוימים שאתם עובדים עליהם, או כספריה להתפתחות וניסיון עם קבצים זמניים.

## איך לעשות זאת?

כאשר מדובר ביצירת קובץ זמני בקלול, ישנן מספר אופציות שניתן להשתמש בהן. לדוגמה, ניתן להשתמש בפונקציה `with-temp-file`, שמקלה על פתיחת קובץ זמני וכן על ניהול הקובץ. נדון יותר בפרטים בעמוד הפירוט "עמוד נחשק".

הנה דוגמא ליצירת קובץ זמני בעזרת `with-temp-file`:

```Clojure
(with-temp-file "/path/to/file"
  (println "Hello, world!"))
```

כאן אנו מגדירים מיקום של הקובץ ומכניסים תוכן לקובץ על-ידי הדפסת המחרוזת "Hello, world!". בסיום הקוד, הקובץ יסגר אוטומטית וימחק.

## עמוד נחשק

היצירת קובץ זמני בקלול היא פתרון יעיל עבור מספר רב של מטרות. כאשר נעשה שימוש נכון, יצירת קובץ זמני יכולה לחסוך זמן ולשפר את חוויית התפתחות הקוד שלכם. ניתן למצוא עוד פרטים על פונקציה `with-temp-file` כאן: [http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/with-temp-file](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/with-temp-file)

## ראו גם

- [ארכיון המאמרים הרשמי של קולז'ר לעברית](https://www.clojure.org/guides/learn/)
- [המדריך העמוק של קולז'ר לעברית](https://github.com/bbatsov/clojure-style-guide)
- [קולז'ר לעברית - עמוד ה-Wikipedia הרשמי](https://he.wikipedia.org/wiki/%D7%A7%D7%95%D7%9C
---
date: 2024-01-26 01:18:26.020645-07:00
description: "\u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05D4\u05D5\
  \u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E9\u05D9\u05E0\u05D5\u05D9\
  \ \u05DE\u05D1\u05E0\u05D4 \u05E7\u05D5\u05D3 \u05DE\u05D7\u05E9\u05D1 \u05E7\u05D9\
  \u05D9\u05DD \u05DE\u05D1\u05DC\u05D9 \u05DC\u05E9\u05E0\u05D5\u05EA \u05D0\u05EA\
  \ \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\u05EA\u05D5 \u05D4\u05D7\u05D9\u05E6\u05D5\
  \u05E0\u05D9\u05EA, \u05E2\u05DD \u05D4\u05DE\u05D8\u05E8\u05D4 \u05DC\u05E9\u05E4\
  \u05E8 \u05D0\u05D8\u05E8\u05D9\u05D1\u05D9\u05D5\u05D8\u05D9\u05DD \u05DC\u05D0\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D9\u05DD. \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\u05DD \u05E8\
  \u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05DB\u05D3\u05D9\u2026"
lastmod: 2024-02-19 22:04:57.990127
model: gpt-4-0125-preview
summary: "\u05E8\u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05D4\u05D5\
  \u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E9\u05D9\u05E0\u05D5\u05D9\
  \ \u05DE\u05D1\u05E0\u05D4 \u05E7\u05D5\u05D3 \u05DE\u05D7\u05E9\u05D1 \u05E7\u05D9\
  \u05D9\u05DD \u05DE\u05D1\u05DC\u05D9 \u05DC\u05E9\u05E0\u05D5\u05EA \u05D0\u05EA\
  \ \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\u05EA\u05D5 \u05D4\u05D7\u05D9\u05E6\u05D5\
  \u05E0\u05D9\u05EA, \u05E2\u05DD \u05D4\u05DE\u05D8\u05E8\u05D4 \u05DC\u05E9\u05E4\
  \u05E8 \u05D0\u05D8\u05E8\u05D9\u05D1\u05D9\u05D5\u05D8\u05D9\u05DD \u05DC\u05D0\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D9\u05DD. \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\u05DD \u05E8\
  \u05D9\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05DB\u05D3\u05D9\u2026"
title: "\u05E8\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2"
---

{{< edit_this_page >}}

## מה ולמה?

ריפקטורינג הוא תהליך של שינוי מבנה קוד מחשב קיים מבלי לשנות את התנהגותו החיצונית, עם המטרה לשפר אטריביוטים לא פונקציונליים. מתכנתים מבצעים ריפקטורינג כדי להפוך את הקוד שלהם לנקי יותר, יעיל יותר וקל יותר לתחזוקה, מה שבפועל משפר את הקריאות ומפחית את המורכבות של התוכנה שלהם.

## איך לעשות:

ריפקטורינג ב-Clojure—בזכות הסינטקס הנקי והפרדיגמה הפונקציונלית—יכול להיות ישיר להפליא. בואו נתמודד עם תרחיש נפוץ: חישוב על קולקציות. ייתכן שתתחילו עם לולאת `for`, כך:

```clojure
(defn calculate-sum [numbers]
  (reduce + 0 numbers))

(defn old-way []
  (let [nums (range 1 11)]
    (calculate-sum nums)))
```

קריאה ל-`(old-way)` תחזיר לנו 55, שהוא הסכום מ-1 עד 10. אך, היי, אנחנו יכולים לבצע ריפקטורינג לכך כדי שיהיה יותר ברוח ה-Clojure:

```clojure
(defn new-way []
  (->> (range 1 11)
       (reduce +)))
```

הפונקציה המרופקטרת `(new-way)` משתמשת במקרוי חישוב רציף כדי להעביר את הטווח ישירות ל-`reduce`, מורידה את העודף.

## עומק יותר

אמנות הריפקטורינג שורשת בימי התחלת פיתוח התוכנה אך זכתה לתנופה אמיתית עם פרסום הספר החשוב של מרטין פאולר "Refactoring: Improving the Design of Existing Code" בשנת 1999. ב-Clojure, ריפקטורינג לעיתים נשען על עקרונות התכנות הפונקציונלית, בעדיפות לפונקציות טהורות ומבני נתונים בלתי ניתנים לשינוי.

תחליפים לריפקטורינג ידני ב-Clojure עשויים לכלול שימוש בכלים כמו Cursive, תוסף פופולרי ל-IntelliJ IDEA, המציע ריפקטורינגים אוטומטיים ספציפיים ל-Clojure. יש גם את clj-refactor, חבילת Emacs עבור Clojure, המספקת מערכת של פונקציות ריפקטורינג.

אתגר ייחודי של ריפקטורינג ב-Clojure הוא התמודדות עם מצב והשפעות צד בפרדיגמה שבעיקרה אימוטבילית וחפה מהשפעות צד. שימוש זהיר באטומים, רפסים, אייג'נטים וטרנזיאנטים קריטי בשמירה על הביצועים והנכונות במהלך ריפקטורינגים.

## ראה גם

- "Refactoring: Improving the Design of Existing Code" מאת מרטין פאולר עבור הקונספטים הבסיסיים.
- [Clojure Docs](https://clojuredocs.org/) עבור דוגמאות ספציפיות של קוד Clojure אידיומטי.
- [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el) לאוטומציה של ריפקטורינג ב-Emacs.
- [Cursive](https://cursive-ide.com/) למשתמשי IntelliJ המחפשים סיוע אוטומטי בריפקטורינג.
- [ריפקטורינג עם ריץ' היקי](https://www.infoq.com/presentations/Simple-Made-Easy/) - הרצאה מאת בורא Clojure, שאמנם לא עוסקת בריפקטורינג כשלעצמה, אך מספקת תובנות על הפילוסופיה של Clojure אשר יכולה להדריך החלטות ריפקטורינג יעילות.

---
title:                "Clojure: לבדוק האם קובץ מאותחל קיים"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# למה

בדיקת קיום תיקייה יכולה להיות תהליך בסיסי בתכנות בשפת Clojure. דרך זו תכולת חיונית בכדי לוודא שהתיקייה שהתוכנית שלנו מנסה לגשת אליה קיימת וזמינה.

## איך לבדוק אם תיקייה קיימת

הבדיקה נעשית באמצעות הפונקציה `exists?` בתוך המארחון של `clojure.java.io`. המארחון הזה מכיל ספריות לטיפול בקבצים ותיקיות.

```Clojure
(ns my-namespace
  (:require [clojure.java.io :as io]))

(defn check-directory [directory]
  (io/exists? directory))
```

אנו משתמשים בפונקציה `exists?` כדי לבדוק אם תיקייה קיימת או לא, והפונקציה מחזירה ערך בוליאני (true או false) בהתאם.

```Clojure
(check-directory "path/to/directory") ;; => true
(check-directory "non-existent/directory") ;; => false
```

## הערות נסתרות

כדי להמשיך לבדוק את תיקיית הפקטורי, נוכל להשתמש בפונקציה `list-files` לבדיקת קבצים בתוך התיקייה. פונקציה זו מקבלת כפרמטר את התיקייה ומחזירה רשימת קבצים בפנים תיקייה.

הנה דוגמה לאיך נוכל לבדוק את התיקייה תוך שימוש בפונקציה `list-files`:

```Clojure
(defn check-directory [directory]
  (and (io/exists? directory) (seq (io/list-files directory))))
```

בהרחבה, אם אנו מעונים לבדוק גם אם התיקייה היא טכנית, אפשר להשתמש בפונקציה `file?` כדי לבדוק אם התוכנית מנסה לגשת לתיקייה או לקובץ.

## חפירה עמוקה

אחת הדרכים הנפוצות לבחור באיזו תיקייה תגש תוכנית היא להשתמש בפונקציה `resolve` כדי למצוא נתיב יתר דין על ידי קבלת התיקייה כקלט.

למשל, אם אנו רוצים לברר נתיב יתר דין לתיקייה "src
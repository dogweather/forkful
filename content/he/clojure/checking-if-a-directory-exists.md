---
title:                "בדיקת קיום תיקייה"
html_title:           "Clojure: בדיקת קיום תיקייה"
simple_title:         "בדיקת קיום תיקייה"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בעולם התכנות, ייתכן שנתקלתם במצב שבו עליכם לבדוק האם תיקייה מסוימת קיימת במערכת הקוד שלכם. התהליך הזה ידוע כבדיקת קיום תיקייה והוא מאפשר לפרוגרמים לבצע פעולות שונות בהתאם לכך אם התיקייה קיימת או לא. זהו כלי חשוב ונחוץ לכל מתכנת שרוצה לכתוב קוד יעיל ובטוח.

## איך לעשות?
תיקיית הקוד הבאה מראה דוגמאות לשימוש בפונקציה המובנת 'exists?' בשפת קלוז'ור כדי לבדוק קיום תיקייה במערכת הקבצים שלנו. אתם יכולים לשנות את הנתיב לפי התיקייה שאתם רוצים לבדוק.

```Clojure
;; Import the 'exists?' function
(import '[java.io File])
;; Define the directory path
(def dir "/Users/username/documents")
;; Check if the directory exists
(print (exists? (File. dir)))
```
מחזיר: true or false

בדוגמה השנייה, אנו מבצעים בדיקה אם תיקייה קיימת בכתובת האתר שלנו. אנחנו יכולים להשתמש בפונקציה 'exists?' עם כתובת האתר כפרמטר.

```Clojure
;; Import the 'exists?' function
(import '[java.io File])
;; Define the website URL
(def url "https://www.example.com")
;; Check if the directory exists
(print (exists? (File. url)))
```
מחזיר: true or false

## חפירה עמוקה
לפני שמתחילים לבדוק קיום תיקייה, כדאי לבחון אם יש פתרונות אחרים שעשויים להיות יעילים יותר. עליכם לקחת בחשבון את סוג הקוד שאתם מפתחים ורמת הביצועים שאתם צריכים.

אם אתם משתמשים בקלוז'ור כשפת תכנות העיקרית שלכם, והשימוש בפונקציה 'exists?' מספק לכם את התוצאה הרצויה, עליכם להמשיך להשתמש בפונקציה הזו. אם אתם משתמשים בשפה נוספת, כדאי לחפש פתרונות אחרים מתאימים לטכנולוגיות השונות.

פונקציית 'exists?' מאפשרת גם לבדוק קיום קבצים או קישורים אחרים, לא רק תיקיות. אתם יכולים למצוא מידע נוסף ודוגמאות אחרות על הפונקציה במקורות הקשורים שנמצאים בסעיף "ראה גם".

## ראה גם
- [Clojure 'exists?' ניתוח חדשני](https://www.baeldung.com/clojure-exists)
- [פונקציית 'exists?' על ClojureDocs](https://clojuredocs.org/clojure.java.io/exists_qmark_)
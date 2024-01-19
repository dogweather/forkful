---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Gleam: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם ספריה קיימת היא ביצוע צ'ק־אפ שמאפשר לנו לבדוק אם ספריה מסוימת נמצאת במערכת הקבצים שלנו. מתכנתים עושים את זה כדי למנוע שגיאות בהרצת התוכנית.

## תרגל:
אז איך אפשר לבדוק את זה ב-Gleam? גם זה די ישר שידורי:

```Gleam
import gleam/filesystem.{Dir}
...
case Dir.exists("הדרך שאל/הספריה") {
    Ok(True) -> אז אנחנו יודעים שהספריה קיימת.
    _ -> ...
}  
```

מה הפלט של זה?

```Gleam
אז אנחנו יודעים שהספריה קיימת.
```

## צלילה עמוקה:
בהקשר ההיסטורי, בדיקת קיום ספריה היא טכניקה מזה "יום א'" של מערכות הפעלה. ב-Gleam, אנחנו מניחים שנכתוב קוד מאובטח, לכן אנחנו קודם מבצעים בדיקות אלה.
לגבי חלופות, ישנן שפות אחרות כמו Node.js ו-Python שעושות את זה בדרכים שונות, אך המימוש ב-Gleam די ברור ויעיל.
בנוגע לפרטי המימוש, `Dir.exists` משתמשת בפקודת `stat` של מערכת ההפעלה לבדוק את מערכת הקבצים.

## ראה גם:
* פוסט בבלוג Gleam's Blog על עבודה עם מערכת הקבצים: https://blog.gleam.run/working-with-filesystem
* מדריך Gleam's הרשמי על מערכת הקבצים: https://gleam.run/guides/filesystem/

חשוב לזכור שהרבה מהפתרונות הללו לא מבטיחים פונקציונליות אטומית.
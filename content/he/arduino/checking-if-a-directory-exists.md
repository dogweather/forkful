---
title:                "Arduino: בדיקת קיום תיקייה"
simple_title:         "בדיקת קיום תיקייה"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

למה: בדיקת אם ספרייה קיימת יכולה להיות חשובה כאשר אתה מפתח תוכניות בארדואינו ומעוניין לוודא שהקוד שלך יעבוד נכון ולא יכשל בפני תנאים לא צפויים.

איך לעשות זאת: כדי לבדוק אם ספרייה מסוימת קיימת בארדואינו, ניתן להשתמש בפונקציית `exists()` ולהעביר לה את הנתיב של הספרייה כפרמטר. לדוגמה:

```Arduino
if (SD.exists("/data")) {
  Serial.print("/data directory exists.");
}
```

### נפילה עמוקה: 
הרשימה הבאה מציגה מידע מעמיק יותר על אופן פעולת בדיקת קיומה של ספרייה בארדואינו:

- פונקציית `exists()` מחזירה ערך בוליאני (true/false) אם הספרייה קיימת או לא.
- ניתן להשתמש גם בפונקציות נוספות כגון `SD.mkdir()` ו- `SD.remove()` כדי ליצור ולמחוק ספריות במצב קיים.
- כדי להשתמש בפונקציית `exists()` יש לוודא שהספרייה מעודכנת וקיימת. במידה ואין את התנאי הנכון, ניתן להשתמש בלולאת `while()` על מנת למתוח את הפעולה עד שהספרייה תימצא.

### ראה גם: 
- [מדריך למתחילים על קבצים וספריות בארדואינו](https://www.arduino.cc/en/Tutorial/Files)
- [דוגמאות לתכנות בארדואינו עם פונקציות SD](https://www.electronicwings.com/arduino/sd-card-interfacing-with-arduino)
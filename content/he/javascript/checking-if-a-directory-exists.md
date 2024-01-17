---
title:                "לבדיקה האם תיקייה קיימת"
html_title:           "Javascript: לבדיקה האם תיקייה קיימת"
simple_title:         "לבדיקה האם תיקייה קיימת"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
בדיקה אם תיקייה קיימת היא פעולה שמאפשרת למתכנתים לבדוק אם תיקייה מסוימת קיימת במחשבם. מתכנתים מבצעים פעולת זו כדי לוודא שהם מבצעים פעולות על תיקייה שקיימת ולא על תיקייה שלא קיימת.

## איך לבצע?
הנה כמה דוגמאות קוד ופלט כדי להבין איך לבדוק אם תיקייה קיימת באמצעות קוד Javascript.
```Javascript
const fs = require('fs');
const path = '/home/user/my_folder';

// לבדוק אם התיקייה קיימת
if (fs.existsSync(path)) {
  console.log('התיקייה קיימת');
} else {
  console.log('התיקייה לא קיימת');
}

// לבדוק אם התיקייה תחת תיקיית מסוימת קיימת
if (fs.existsSync('/home/user/my_folder/my_subfolder')) {
  console.log('התיקייה קיימת בתוך התיקייה הנתונה');
} else {
  console.log('התיקייה לא קיימת בתוך התיקייה הנתונה');
}
```

## חקירה עמוקה
בעבר, בדיקה אם תיקייה קיימת הייתה עיקרונית חלק ממגוון שפות תכנות, אך עכשיו בפעילות המתודולוגיה, פעולה זו נחשבת לפעולה סטנדרטית בכללות. יתר על כן, ישנן גם אפשרויות נוספות לבדוק אם תיקייה קיימת, כגון באמצעות פקודת ה-Terminal או בכלי פתוח כמו Node.js. כאשר מבצעים פעולות על קבצים או תיקיות בקוד, חשוב לוודא שהם קיימים על מנת למנוע שגיאות בזמן ריצת התכנית.

## ראי גם
כתב העת זה מציע מידע נרחב יותר על בדיקת קיום תיקיות ב-Javascript. ניתן למצוא גם דוגמאות נוספות והסברים על מנגנונים נוספים לבדיקה זו. אתר זה מכיל גם מידע על כלים נוספים שניתן להשתמש בהם כדי לבדוק קיום תיקיות בקוד עבור פלטפורמות שונות. https://www.digitalocean.com/community/tutorials/how-to-check-if-a-directory-exists-in-a-filesystem-in-node-js
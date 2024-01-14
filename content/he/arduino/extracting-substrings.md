---
title:    "Arduino: חילוץ תת מחרוזתות"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

ברוך הבא לכתבה על תכנות ארדואינו! היום אנו מתחשק לחלוק אתכם מהעולם המעניין של הפצת מחרוזות בתכנת ארדואינו. כל מה שאתם צריכים כדי ללמוד איך לבצע את הפעולה הזו מדוגמים ומידע עמוק על עולם המחרוזות. בואו נתחיל!

# למה

למה נרצה לחלץ מחרוזות? למעשה, עיבוד מחרוזות הוא חלק חשוב מתכנן ארדואינו. זה יכול לעזור לנו לקלט נתונים מסוגים שונים ולהטביע עדכונים למפשקים שונים כמו LCD תצוגת הדיגיטל הכרטיסים SD.

## איך לעשות

דוגמא לחילוץ מחרוזות והדפסתן:

```arduino
// חילוץ מחרוזת
String message = "שלום מכללה טכנולוגית";
String substring = message.substring(6, 13); // מחזיר את הכיתוב "מכללה"
Serial.println(substring); // מדפיס "מכללה" לרכיב החומרה

// מחלק את המחרוזת לחלקים ומדפיס אותם
String message2 = "אחת,שתיים,שלוש";
String buf[3];
message2.split(",", buf, 3);
Serial.println(buf[0]); // מדפיס "אחת"
Serial.println(buf[1]); // מדפיס "שתיים"
Serial.println(buf[2]); // מדפיס "שלוש"
```

## הכנסה

איך ארדואינו מתפעל את פעולת החילוץ של מחרוזות? כאשר המחרוזת מגיעה כבקשת קלט בזיכרון חשיפה - היא פותחת את הריכוז מבקשת CRUD (תכנות יצירה, קריאה, עידכוני מחרוזות) באמצעות תכונה המתאימה.

# ראה גם

איך לגרום לארדואינו לקרוא נתונים מקלטים חיצוניים: [https://www.mathworks.com/help/supportpkg/arduinoio/ug/read-and-write-data.html](https://www.mathworks.com/help/supportpkg/arduinoio/ug/read-and-write-data.html)

המהדר USB הנתמך על ידי תוכנת Arduino: [https://www.arduino.cc/en/software](https://www.arduino.cc/en/software)

הרכיב של טבילונה ש
---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:51.323856-07:00
description: "\u05DC\u05D4\u05D5\u05DF \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E4\u05D9\u05E8\u05D5\u05E9\u05D5 \u05DC\
  \u05D4\u05DE\u05D9\u05E8 \u05D0\u05EA \u05D4\u05EA\u05D5 \u05D4\u05E8\u05D0\u05E9\
  \u05D5\u05DF \u05E9\u05DC \u05DB\u05DC \u05DE\u05D9\u05DC\u05D4 \u05D1\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4\
  , \u05EA\u05D5\u05DA \u05DB\u05D3\u05D9 \u05E9\u05D0\u05E8 \u05D4\u05D0\u05D5\u05EA\
  \u05D9\u05D5\u05EA \u05D9\u05E9\u05D0\u05E8\u05D5 \u05E7\u05D8\u05E0\u05D5\u05EA\
  . \u05E4\u05E2\u05D5\u05DC\u05D4 \u05D6\u05D5 \u05E0\u05E4\u05D5\u05E6\u05D4 \u05D1\
  \u05E2\u05D9\u05E6\u05D5\u05D1 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D5\u05E0\
  \u05E8\u05DE\u05D5\u05DC \u05E7\u05DC\u05D8\u2026"
lastmod: '2024-03-13T22:44:39.739911-06:00'
model: gpt-4-0125-preview
summary: "\u05DC\u05D4\u05D5\u05DF \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05E4\u05D9\u05E8\u05D5\u05E9\u05D5 \u05DC\u05D4\
  \u05DE\u05D9\u05E8 \u05D0\u05EA \u05D4\u05EA\u05D5 \u05D4\u05E8\u05D0\u05E9\u05D5\
  \u05DF \u05E9\u05DC \u05DB\u05DC \u05DE\u05D9\u05DC\u05D4 \u05D1\u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4, \u05EA\
  \u05D5\u05DA \u05DB\u05D3\u05D9 \u05E9\u05D0\u05E8 \u05D4\u05D0\u05D5\u05EA\u05D9\
  \u05D5\u05EA \u05D9\u05E9\u05D0\u05E8\u05D5 \u05E7\u05D8\u05E0\u05D5\u05EA. \u05E4\
  \u05E2\u05D5\u05DC\u05D4 \u05D6\u05D5 \u05E0\u05E4\u05D5\u05E6\u05D4 \u05D1\u05E2\
  \u05D9\u05E6\u05D5\u05D1 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D5\u05E0\u05E8\
  \u05DE\u05D5\u05DC \u05E7\u05DC\u05D8\u2026"
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
להון אותיות במחרוזת פירושו להמיר את התו הראשון של כל מילה במחרוזת לאות גדולה, תוך כדי שאר האותיות ישארו קטנות. פעולה זו נפוצה בעיצוב נתונים ונרמול קלט משתמש כדי לשמור על עקביות ולשפר קריאות.

## איך לעשות:
ארדואינו, שמוכר בעיקר בהתקשותו עם חומרה, כולל גם יכולות בסיסיות לעריכת מחרוזות דרך אובייקט ה`String` שלו. עם זאת, הוא חסר פונקציה ישירה להונה כפי שנראית בשפות ברמה גבוהה יותר. לכן, אנחנו מיישמים הונה על ידי חיזור על מחרוזת ויישום שינויי האותיות.

הנה דוגמה בסיסית ללא שימוש בספריות צד שלישי:

```cpp
String capitalizeString(String input) {
  if (input.length() == 0) {
    return ""; // חזרה של מחרוזת ריקה אם הקלט ריק
  }
  input.toLowerCase(); // המרה של כל המחרוזת לאותיות קטנות תחילה
  input.setCharAt(0, input.charAt(0) - 32); // הונת התו הראשון
  
  // הונת אותיות שבאות אחרי רווח
  for (int i = 1; i < input.length(); i++) {
    if (input.charAt(i - 1) == ' ') {
      input.setCharAt(i, input.charAt(i) - 32);
    }
  }
  return input;
}

void setup() {
  Serial.begin(9600);
  String testStr = "hello arduino world";
  String capitalizedStr = capitalizeString(testStr);
  Serial.println(capitalizedStr); // פלט: "Hello Arduino World"
}

void loop() {
  // לולאה ריקה
}
```

קטע הקוד הזה מגדיר פונקציה שנקראת `capitalizeString` שקודמת על המרת כל המחרוזת לאותיות קטנות כדי לאחד את האותיות. לאחר מכן, היא מבצעת הונה של התו הראשון וכל תו שבא אחרי רווח, שבעצם מבצעת הונה של כל מילה במחרוזת הקלט. שימו לב שמימוש זה נחשב לבסיסי והוא מניח קידוד תווים ASCII ועשוי לדרוש התאמות לתמיכה מלאה ביוניקוד.

כרגע, אין ספריות צד שלישי שנתפסו באופן רחב כיעד לעריכת מחרוזות באקוסיסטם של ארדואינו, בעיקר בגלל התמקדותו באינטרקציה עם חומרה ויעילות. עם זאת, הדוגמה המסופקת היא דרך ישירה להשגת הונת מחרוזות בסביבת התכנות של ארדואינו.

---
title:                "הגדלת אותיות במחרוזת"
aliases:
- he/arduino/capitalizing-a-string.md
date:                  2024-02-03T19:05:51.323856-07:00
model:                 gpt-4-0125-preview
simple_title:         "הגדלת אותיות במחרוזת"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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

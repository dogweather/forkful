---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:11.560584-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Arduino \u05D0\
  \u05D9\u05DF \u05EA\u05DE\u05D9\u05DB\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA\
  \ \u05DC\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\
  \u05D9\u05D9\u05DD \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA \u05D1\u05E1\u05E4\u05E8\
  \u05D9\u05D9\u05EA \u05D4\u05EA\u05E7\u05E0\u05D9\u05EA \u05E9\u05DC\u05D5. \u05E2\
  \u05DD \u05D6\u05D0\u05EA, \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05E9\u05D9\u05D2\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05D3\
  \u05D5\u05DE\u05D4 \u05DC-regex \u05E2\u05D1\u05D5\u05E8 \u05EA\u05D1\u05E0\u05D9\
  \u05D5\u05EA \u05E4\u05E9\u05D5\u05D8\u05D5\u05EA \u05D1\u05D0\u05DE\u05E6\u05E2\
  \u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.750914-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Arduino \u05D0\u05D9\u05DF \u05EA\u05DE\u05D9\u05DB\u05D4 \u05DE\u05D5\
  \u05D1\u05E0\u05D9\u05EA \u05DC\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\
  \u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA\
  \ \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\u05EA\u05E7\u05E0\u05D9\u05EA\
  \ \u05E9\u05DC\u05D5."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD"
weight: 11
---

## איך לעשות:
ב-Arduino אין תמיכה מובנית לביטויים רגולריים ישירות בספריית התקנית שלו. עם זאת, ניתן להשיג פונקציונליות דומה ל-regex עבור תבניות פשוטות באמצעות פונקציות בסיסיות של מחרוזות, או לצרכים מורכבים יותר, לשלב ספרייה צד שלישי כמו `regex`.

### התאמת מחרוזות בסיסית ללא Regex
לצרכים בסיסיים, כמו מציאת תת-מחרוזת, ניתן להשתמש בפונקציה `String.indexOf()`:
```cpp
String data = "Sensor value: 12345";
int index = data.indexOf("value:");
if (index != -1) {
  String value = data.substring(index + 6).trim();
  Serial.println(value); // מוציא: 12345
}
```

### שימוש בספרייה צד שלישי ל-Regex
לניהול תבניות מורכבות יותר, כדאי לשקול ספרייה כמו `regex`. לאחר התקנת הספרייה, ניתן להשתמש בה כדלקמן:

1. **התקנה**: ספריית ה-`regex` עשויה שלא להיות זמינה במאגר הספריות של Arduino, ולכן ייתכן שתצטרך להתקין אותה ידנית על ידי הורדה ממקור אמין והוספתה לתיקיית הספריות של Arduino שלך.

2. **דוגמה לשימוש**:
בהנחה שהספרייה מספקת פונקציונליות דומה ליישומי regex סטנדרטיים, ייתכן שתשתמש בה כך:

```cpp
#include <regex.h>

void setup() {
  Serial.begin(9600);
  while (!Serial); // המתן עד שהסריאל יהיה מוכן
  
  regex_t reg;
  const char* pattern = "[0-9]+"; // מתאים לסדרה של ספרות
  regcomp(&reg, pattern, REG_EXTENDED);
  
  const char* test_str = "Sensor value: 12345";
  
  regmatch_t matches[1];
  if (regexec(&reg, test_str, 1, matches, 0) == 0) {
    // חילוץ והדפסה של החלק התואם
    int start = matches[0].rm_so;
    int end = matches[0].rm_eo;
    char match[end-start+1];
    strncpy(match, test_str + start, end-start);
    match[end-start] = '\0';
    
    Serial.print("נמצאה התאמה: ");
    Serial.println(match); // מוציא: 12345
  } else {
    Serial.println("לא נמצאה התאמה");
  }
  
  regfree(&reg); // שחרור הזיכרון שהוקצה עבור ה-regex
}

void loop() {
  // הכנס את הקוד העיקרי שלך כאן, להרצה חוזרת ונשנית:
}
```

**הערה**: התחביר והפונקציות הספציפיות שהוצגו כאן הן לצורך המחשה בלבד ועשויות להשתנות בהתאם לפרטי היישום המדויקים של ספריית ה-`regex` שתבחר. בכל מקרה, מומלץ תמיד לעיין בתיעוד של הספרייה למידע מדויק ומעודכן.

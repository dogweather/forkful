---
title:                "שימוש בביטויים רגולריים"
aliases:
- /he/arduino/using-regular-expressions.md
date:                  2024-02-03T19:17:11.560584-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש בביטויים רגולריים"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
ביטויים רגולריים (regex) הם סדרות של תווים המגדירות תבניות חיפוש, המשמשות בעיקר להתאמת מחרוזות ולשינויין. מתכנתים מנצלים ביטויים רגולריים בפרויקטים של Arduino לניתוח קלטים סריאליים, ולידציה של קלט מהמשתמש, או לחילוץ נתונים ממחרוזות, מה שמעלה את יעילות וגמישות עיבוד הנתונים.

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

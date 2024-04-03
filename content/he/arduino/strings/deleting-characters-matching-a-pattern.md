---
date: 2024-01-20 17:41:54.038591-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D0\u05E8\u05D3\
  \u05D5\u05D0\u05D9\u05E0\u05D5 \u05DC\u05D0 \u05EA\u05D5\u05DE\u05DA \u05D1\u05D0\
  \u05D5\u05E4\u05DF \u05DE\u05D5\u05D1\u05E0\u05D4 \u05D1\u05D1\u05D9\u05D8\u05D5\
  \u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD. \u05E0\u05E6\
  \u05D8\u05E8\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA \u05D9\u05D3\
  \u05E0\u05D9\u05EA. \u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D0."
lastmod: '2024-03-13T22:44:39.741600-06:00'
model: gpt-4-1106-preview
summary: "\u05D0\u05E8\u05D3\u05D5\u05D0\u05D9\u05E0\u05D5 \u05DC\u05D0 \u05EA\u05D5\
  \u05DE\u05DA \u05D1\u05D0\u05D5\u05E4\u05DF \u05DE\u05D5\u05D1\u05E0\u05D4 \u05D1\
  \u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\
  \u05DD."
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
weight: 5
---

## איך לעשות:
ארדואינו לא תומך באופן מובנה בביטויים רגולריים. נצטרך לעשות זאת ידנית. הנה דוגמא:

```Arduino
String removePattern(String str, char toRemove) {
  String result = "";
  for (unsigned int i = 0; i < str.length(); i++) {
    if (str[i] != toRemove) result += str[i];
  }
  return result;
}

void setup() {
  Serial.begin(9600);
  String data = "Hello-World-123";
  Serial.println(removePattern(data, '-'));  // "HelloWorld123"
}

void loop() {
  // Nothing to do here
}
```

פלט הדוגמה: "HelloWorld123"

## עיון מעמיק:
עד לשנות ה-70, כאשר עיבוד טקסט התחיל להיות נפוץ יותר, לא הייתה דרך פשוטה להסיר תווים לפי תבנית. ביטויים רגולריים, שנוצרו בשנת 1956 על ידי סטיבן קליני, שיפרו את היכולת לחפש ולעבד טקסט באופן יעיל. קוד הארדואינו נועד להיות פשוט ואינו כולל תמיכה רשמית בביטויים רגולריים, מכיוון שלוקח הרבה משאבים במערכת עם משאבים חסוכים. דרך חלופית היא להשתמש בספריות צד שלישי שנועדו לארדואינו, אך יש לשים לב לשימוש בזיכרון ובמשאבי מעבד.

## ראה גם:
- [מדריך ארדואינו רשמי](https://www.arduino.cc/reference/en/)
- [דוקומנטציה למחרוזות בארדואינו](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [פורום התמיכה של ארדואינו](https://forum.arduino.cc/)
- [ביטויים רגולריים](https://en.wikipedia.org/wiki/Regular_expression) (להבנה כללית, לא ספציפית לארדואינו)

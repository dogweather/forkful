---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:18.286127-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DC\u05D0\u05E8\
  \u05D3\u05D5\u05D0\u05D9\u05E0\u05D5 \u05D0\u05D9\u05DF \u05DE\u05E1\u05D2\u05E8\
  \u05EA \u05D1\u05D3\u05D9\u05E7\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DB\
  \u05DE\u05D5 \u05D1\u05E1\u05D1\u05D9\u05D1\u05D5\u05EA \u05EA\u05DB\u05E0\u05D5\
  \u05EA \u05D0\u05D7\u05E8\u05D5\u05EA. \u05E2\u05DD \u05D6\u05D0\u05EA, \u05E0\u05D9\
  \u05EA\u05DF \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\
  \u05D5\u05EA \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9 \u05DB\u05DE\u05D5 `AUnit`\
  \ \u05DC\u05D1\u05D3\u05D9\u05E7\u05EA \u05D9\u05D7\u05D9\u05D3\u05D4 \u05E9\u05DC\
  \ \u05E7\u05D5\u05D3 \u05D0\u05E8\u05D3\u05D5\u05D0\u05D9\u05E0\u05D5. AUnit\u2026"
lastmod: '2024-03-13T22:44:39.772052-06:00'
model: gpt-4-0125-preview
summary: "\u05DC\u05D0\u05E8\u05D3\u05D5\u05D0\u05D9\u05E0\u05D5 \u05D0\u05D9\u05DF\
  \ \u05DE\u05E1\u05D2\u05E8\u05EA \u05D1\u05D3\u05D9\u05E7\u05D4 \u05DE\u05D5\u05D1\
  \u05E0\u05D9\u05EA \u05DB\u05DE\u05D5 \u05D1\u05E1\u05D1\u05D9\u05D1\u05D5\u05EA\
  \ \u05EA\u05DB\u05E0\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA"
weight: 36
---

## איך לעשות:
לארדואינו אין מסגרת בדיקה מובנית כמו בסביבות תכנות אחרות. עם זאת, ניתן להשתמש בספריות צד שלישי כמו `AUnit` לבדיקת יחידה של קוד ארדואינו. AUnit מושרה מהספריה המובנית של ארדואינו, `ArduinoUnit`, וממסגרת הבדיקות של גוגל, `Google Test`.

### דוגמה עם AUnit:
ראשית, התקן את AUnit דרך מנהל הספריות בסביבת פיתוח הארדואינו: עבור אל Sketch > Include Library > Manage Libraries... > חפש את AUnit והתקן אותו.

לאחר מכן, תוכל לכתוב בדיקות כך:

```cpp
#include <AUnit.h>

test(ledPinHigh) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, HIGH);
  assertTrue(digitalRead(ledPin));
}

test(ledPinLow) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, LOW);
  assertFalse(digitalRead(ledPin));
}

void setup() {
  Serial.begin(9600);
  aunit::TestRunner::run();
}

void loop() {
  // ריק
}
```
לאחר העלאת בדיקה זו ללוח הארדואינו שלך, פתח את המוניטור הסריאלי כדי להציג את תוצאות הבדיקה. עליך לראות פלט המציין אם כל בדיקה עברה או נכשלה:

```
TestRunner התחיל על 2 בדיקות.
Test ledPinHigh עבר.
Test ledPinLow עבר.
משך הריצה של TestRunner: 0.002 שניות.
סיכום TestRunner: 2 עברו, 0 נכשלו, 0 דולגו, 0 הסתיימו בזמן, מתוך 2 בדיקות.
```

דוגמה זו הפשוטה מדגימה את השימוש ב-AUnit לבדיקת מצב של פין LED. על ידי יצירת בדיקות, אתה מאשר שהארדואינו שלך מתנהג כצפוי בתנאים שונים. עם AUnit, ניתן לכתוב בדיקות מורכבות יותר, ערכות בדיקה וליהנות מתכונות כמו פקודות זמן תפוגה לבדיקות והכנות/פירוק נהלים לתרחישים מתקדמים יותר.

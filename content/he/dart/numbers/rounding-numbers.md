---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:38.419254-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Dart \u05DE\u05E1\
  \u05E4\u05E7\u05EA \u05E9\u05D9\u05D8\u05D5\u05EA \u05D9\u05DC\u05D9\u05D3\u05D9\
  \u05D5\u05EA \u05D1\u05E1\u05D5\u05D2 `num` \u05D4\u05DE\u05E8\u05DB\u05D6\u05D9\
  \ \u05DC\u05D1\u05D9\u05E6\u05D5\u05E2 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05E2\
  \u05D9\u05D2\u05D5\u05DC. \u05DB\u05D0\u05DF, \u05E0\u05D7\u05E7\u05D5\u05E8 \u05E9\
  \u05D9\u05D8\u05D5\u05EA \u05DB\u05DE\u05D5 `round()`, `floor()`, `ceil()`, \u05D5\
  \u05D0\u05D9\u05DA \u05DC\u05E2\u05D2\u05DC \u05DC\u05DE\u05E1\u05E4\u05E8 \u05DE\
  \u05E1\u05D5\u05D9\u05DD \u05E9\u05DC\u2026"
lastmod: '2024-03-13T22:44:38.836719-06:00'
model: gpt-4-0125-preview
summary: "Dart \u05DE\u05E1\u05E4\u05E7\u05EA \u05E9\u05D9\u05D8\u05D5\u05EA \u05D9\
  \u05DC\u05D9\u05D3\u05D9\u05D5\u05EA \u05D1\u05E1\u05D5\u05D2 `num` \u05D4\u05DE\
  \u05E8\u05DB\u05D6\u05D9 \u05DC\u05D1\u05D9\u05E6\u05D5\u05E2 \u05E4\u05E2\u05D5\
  \u05DC\u05D5\u05EA \u05E2\u05D9\u05D2\u05D5\u05DC."
title: "\u05E1\u05D9\u05D1\u05D5\u05D1 \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD"
weight: 13
---

## איך לעשות:
Dart מספקת שיטות ילידיות בסוג `num` המרכזי לביצוע פעולות עיגול. כאן, נחקור שיטות כמו `round()`, `floor()`, `ceil()`, ואיך לעגל למספר מסוים של נקודות עשרוניות.

### עיגול לספרה השלמה הקרובה ביותר:
```dart
var number = 3.56;
print(number.round()); // מוציא: 4
```

### עיגול כלפי מטה:
```dart
print(number.floor()); // מוציא: 3
```

### עיגול כלפי מעלה:
```dart
print(number.ceil()); // מוציא: 4
```

### עיגול למספר מסוים של נקודות עשרוניות:
כדי לעגל למספר מסוים של נקודות עשרוניות, אנו יכולים להשתמש בשיטת `toStringAsFixed()`, אשר מחזירה מחרוזת, או להשתמש בשילוב של `pow` מ-`dart:math` לתוצאה מספרית.

```dart
import 'dart:math';

var number = 3.56789;
String roundedString = number.toStringAsFixed(2); // למטרות הצגה
print(roundedString); // מוציא: 3.57

double roundedNumber = double.parse(roundedString);
print(roundedNumber); // מוציא: 3.57

// חלופית, לתוצאה מספרית:
double roundedToDecimal = (number * pow(10, 2)).round().toDouble() / pow(10, 2);
print(roundedToDecimal); // מוציא: 3.57
```

על אף שספריית הליבה של Dart מכסה את רוב צרכי העיגול בצורה יעילה, לפעולות מתמטיות מורכבות יותר או דרישות עיגול מדויקות, ספריות כמו `decimal` יכולות להיות שימושיות. ספריית `decimal` מספקת דרך קלה לעבוד עם מספרים עשרוניים בלי לאבד דיוק, מה שמאוד נוח לחישובים פיננסיים, אך לשיטות עיגול פשוטות כפי שהוצג, הפונקציונליות המרכזית של Dart לרוב מספקת בהחלט.

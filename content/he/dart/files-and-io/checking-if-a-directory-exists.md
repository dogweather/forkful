---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:11.964282-07:00
description: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\
  \u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1-Dart \u05D4\u05D9\u05D0 \u05E2\
  \u05DC \u05DE\u05E0\u05EA \u05DC\u05D5\u05D5\u05D3\u05D0 \u05D0\u05EA \u05E7\u05D9\
  \u05D5\u05DE\u05D4 \u05E9\u05DC \u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 \u05D1\u05E0\
  \u05EA\u05D9\u05D1 \u05DE\u05E1\u05D5\u05D9\u05DD \u05E2\u05DC \u05DE\u05E2\u05E8\
  \u05DB\u05EA \u05D4\u05E7\u05D1\u05E6\u05D9\u05DD \u05DC\u05E4\u05E0\u05D9 \u05D1\
  \u05D9\u05E6\u05D5\u05E2 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05DB\u05DE\u05D5\
  \ \u05E7\u05E8\u05D9\u05D0\u05D4 \u05D0\u05D5 \u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\
  \u05D1\u05E6\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\u2026"
lastmod: '2024-03-13T22:44:38.871101-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1-Dart \u05D4\u05D9\u05D0 \u05E2\u05DC\
  \ \u05DE\u05E0\u05EA \u05DC\u05D5\u05D5\u05D3\u05D0 \u05D0\u05EA \u05E7\u05D9\u05D5\
  \u05DE\u05D4 \u05E9\u05DC \u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 \u05D1\u05E0\u05EA\
  \u05D9\u05D1 \u05DE\u05E1\u05D5\u05D9\u05DD \u05E2\u05DC \u05DE\u05E2\u05E8\u05DB\
  \u05EA \u05D4\u05E7\u05D1\u05E6\u05D9\u05DD \u05DC\u05E4\u05E0\u05D9 \u05D1\u05D9\
  \u05E6\u05D5\u05E2 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05DB\u05DE\u05D5 \u05E7\
  \u05E8\u05D9\u05D0\u05D4 \u05D0\u05D5 \u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D1\
  \u05E6\u05D9\u05DD."
title: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA"
weight: 20
---

## מה ולמה?

בדיקה אם ספרייה קיימת ב-Dart היא על מנת לוודא את קיומה של ספרייה בנתיב מסוים על מערכת הקבצים לפני ביצוע פעולות כמו קריאה או כתיבת קבצים. מתכנתים עושים זאת כדי להימנע משגיאות שמתרחשות בעת ניסיון לגשת או לשנות ספריות שלא קיימות.

## איך לעשות:

Dart משתמש בספריית `dart:io` לעבוד עם קבצים וספריות. הנה דרך פשוטה לבדוק אם ספרייה קיימת:

```dart
import 'dart:io';

void main() {
  var directory = Directory('path/to/your/directory');

  if (directory.existsSync()) {
    print('Directory exists');
  } else {
    print('Directory does not exist');
  }
}
```
פלט דוגמא אם הספרייה קיימת:
```
Directory exists
```

או, אם היא לא קיימת:
```
Directory does not exist
```

לטיפול בתרחישים מורכבים יותר, כמו בדיקה אסינכרונית או יצירת ספרייה אם היא לא קיימת, אפשר להשתמש בגישה הבאה:

```dart
import 'dart:io';

void main() async {
  var directory = Directory('path/to/your/directory');

  // בודק באופן אסינכרוני אם הספרייה קיימת
  var exists = await directory.exists();
  if (exists) {
    print('Directory exists');
  } else {
    print('Directory does not exist, creating...');
    await directory.create(); // זה יוצר את הספרייה
    print('Directory created');
  }
}
```

פלט דוגמא אם הספרייה לא קיימת ונוצרה:
```
Directory does not exist, creating...
Directory created
```

יכולותיו הטבעיות של Dart בדרך כלל מספיקות לטיפול בקבצים ובספריות, כך שלא נדרשות ספריות של צד שלישי למשימה זו. עם זאת, לפעולות מורכבות יותר על מערכת הקבצים, חבילות כמו `path` (למניפולציה של נתיבים בצורה שאיננה תלויה בפלטפורמה) יכולות להשלים את ספריית `dart:io` אך לא מציעות באופן ישיר בדיקות קיום ספרייה מתקדמות יותר ממה שמוצג.

---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:08.631780-07:00
description: "\u05D4\u05D5\u05E4\u05DB\u05D9\u05DD \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D9\u05E9\u05D9\u05D5\u05EA\
  \ \u05E2\u05DC \u05D9\u05D3\u05D9 \u05E9\u05D9\u05E0\u05D5\u05D9 \u05D4\u05D0\u05D5\
  \u05EA \u05D4\u05E8\u05D0\u05E9\u05D5\u05E0\u05D4 \u05E9\u05DC \u05DE\u05D9\u05DC\
  \u05D4 \u05D0\u05D5 \u05DE\u05E9\u05E4\u05D8 \u05E9\u05DC\u05DD \u05DC\u05D0\u05D5\
  \u05EA \u05D2\u05D3\u05D5\u05DC\u05D4, \u05EA\u05D5\u05DA \u05E9\u05DE\u05D9\u05E8\
  \u05D4 \u05E2\u05DC \u05E9\u05D0\u05E8 \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD \u05DB\
  \u05E4\u05D9 \u05E9\u05D4\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\
  \u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05DE\u05E9\
  \u05EA\u05DE\u05E9\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:38.815419-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05D5\u05E4\u05DB\u05D9\u05DD \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E8\u05D9\u05E9\u05D9\u05D5\u05EA\
  \ \u05E2\u05DC \u05D9\u05D3\u05D9 \u05E9\u05D9\u05E0\u05D5\u05D9 \u05D4\u05D0\u05D5\
  \u05EA \u05D4\u05E8\u05D0\u05E9\u05D5\u05E0\u05D4 \u05E9\u05DC \u05DE\u05D9\u05DC\
  \u05D4 \u05D0\u05D5 \u05DE\u05E9\u05E4\u05D8 \u05E9\u05DC\u05DD \u05DC\u05D0\u05D5\
  \u05EA \u05D2\u05D3\u05D5\u05DC\u05D4, \u05EA\u05D5\u05DA \u05E9\u05DE\u05D9\u05E8\
  \u05D4 \u05E2\u05DC \u05E9\u05D0\u05E8 \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD \u05DB\
  \u05E4\u05D9 \u05E9\u05D4\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\
  \u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05DE\u05E9\
  \u05EA\u05DE\u05E9\u05D9\u05DD\u2026"
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

הופכים מחרוזת לאותיות רישיות על ידי שינוי האות הראשונה של מילה או משפט שלם לאות גדולה, תוך שמירה על שאר התווים כפי שהם. מתכנתים לעיתים קרובות משתמשים בטכניקה זו בעת עיצוב קלטים מהמשתמש או הצגת טקסט לצורך אחידות או ציות לכללי דקדוק בממשקי משתמש.

## איך לעשות:

### באמצעות שיטות המובנות של Dart

Dart מספקת שיטות פשוטות וישירות לניפוי מחרוזות. כדי להפוך מילה או משפט לאותיות רישיות, תקח בדרך כלל את התו הראשון, תשנה אותו לאות גדולה, ואז תשרשר אותו עם שאר המחרוזת. הנה איך תוכל ליישם זאת:

```dart
String capitalize(String text) {
  if (text.isEmpty) return text;
  return text[0].toUpperCase() + text.substring(1).toLowerCase();
}

void main() {
  var example = "hello world";
  print(capitalize(example)); // פלט: Hello world
}
```

### הופכים כל מילה לאות רישית

כדי להפוך את האות הראשונה של כל מילה במחרוזת, תוכל לפצל את המחרוזת למילים, להפוך כל אחת מהן לאות רישית, ולאחר מכן לחבר אותן בחזרה יחד:

```dart
String capitalizeWords(String text) {
  return text.split(' ').map(capitalize).join(' ');
}

void main() {
  var example = "hello dart enthusiasts";
  print(capitalizeWords(example)); // פלט: Hello Dart Enthusiasts
}
```

### באמצעות ספריות צד שלישי

למרות שספריית התקנים של Dart כוללת את הצרכים הבסיסיים, מטלות מסוימות יכולות להתבצע בצורה נוחה יותר באמצעות חבילות של צד שלישי. בחירה פופולרית ליכולות ניפוי מחרוזות מורחבות, כולל הפיכה לאותיות רישיות, היא חבילת ה- [`recase`](https://pub.dev/packages/recase). לאחר הוספתה לקובץ `pubspec.yaml` של הפרויקט שלך, תוכל בקלות להפוך מחרוזות לאותיות רישיות בין פונקציונליות אחרות:

```dart
import 'package:recase/recase.dart';

void main() {
  var example = "hello world";
  var rc = ReCase(example);

  print(rc.titleCase); // פלט: Hello World
}
```

באמצעות `recase`, תוכל להפוך מילים יחידות, משפטים שלמים, או אף לעקוב אחרי קונבנציות רישום אחרות בלי לטפל ידנית בהמרות המחרוזת.

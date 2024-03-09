---
title:                "שימוש בביטויים רגילים"
date:                  2024-03-08T21:58:59.674857-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
ביטויים רגולריים (regex) ב-Dart מציעים דרך עוצמתית לחפש ולעבד מחרוזות, מה שמאפשר למתכנתים לבצע משימות עיבוד טקסט מורכבות ביעילות. באמצעות הבנה של regex, מפתחים יכולים לבצע אימותי טקסט, חיפוש דפוסים, והסבות טקסט במהירות, דבר החיוני לעיבוד טפסים, פיענוח נתונים, ומניפולציות מחרוזות כלליות ביישומים מודרניים.

## איך ל:
Dart משתמשת בכיתה `RegExp` עבור ביטויים רגולריים. הנה דוגמא בסיסית להתאמת דפוס פשוט בתוך מחרוזת:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Learning Dart programming is exciting.';

  if (pattern.hasMatch(text)) {
    print('נמצאה התאמה!');
  } else {
    print('לא נמצאה התאמה.');
  }
  // פלט: נמצאה התאמה!
}
```

כדי לחלץ התאמות ממחרוזת, אתה יכול להשתמש בשיטה `allMatches`. שיטה זו מחזירה איטרבל של התאמות:

```dart
void main() {
  var pattern = RegExp(r'\b\w+\b');
  var text = 'Dart is awesome!';

  var matches = pattern.allMatches(text);
  for (final match in matches) {
    print(match.group(0)); // מדפיס את המחרוזות שהתאימו.
  }
  // פלט:
  // Dart
  // is
  // awesome
}
```

החלפת טקסט ניתן להשיג באמצעות השיטות `replaceFirst` או `replaceAll`:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Dart is not just a dart.';
  
  // החלפת המופע הראשון
  var modifiedText = text.replaceFirst(pattern, 'Flutter');
  print(modifiedText); 
  // פלט: Flutter is not just a dart.

  // החלפת כל המופעים
  modifiedText = text.replaceAll(pattern, 'Flutter');
  print(modifiedText);
  // פלט: Flutter is not just a flutter.
}
```

פיצול מחרוזת לפי דפוס regex הוא פשוט באמצעות שימוש בשיטת `split`:

```dart
void main() {
  var pattern = RegExp(r'\s+'); // מתאים לכל תווי רווח
  var text = 'Dart is fun';

  var parts = text.split(pattern);
  print(parts); 
  // פלט: [Dart, is, fun]
}
```

לעיבוד או אימותים מורכבים שלא נתמכים ישירות על ידי `RegExp` של Dart, ייתכן שתרצה לשקול ספריות צד שלישי, אך ספריית הסטנדרט של Dart לעיתים קרובות מספיקה למשימות regex נפוצות, תוך הדגשת השימושיות והגמישות שלה בטיפול בביטויים רגולריים.

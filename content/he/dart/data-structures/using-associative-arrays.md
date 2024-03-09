---
title:                "שימוש במערכים אסוציאטיביים"
date:                  2024-03-08T21:57:27.347185-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

מערכי אסוציאציות ב-Dart, הידועות כמפות (Maps), הן מבני נתונים ששומרים נתונים בזוגות מפתח-ערך. הן מאפשרות למתכנתים לגשת לאלמנטים לא דרך אינדקסים אלא דרך מפתחות, מה שהופך את איתור הנתונים לאינטואיטיבי ויעיל, במיוחד כאשר עובדים עם נתונים מובנים שבהם לכל אלמנט יש זיהוי ייחודי.

## איך עושים:

Dart מציע תחביר פשוט ליצירה והתעסקות עם Maps. להלן דוגמאות המדגימות פעולות בסיסיות כמו יצירה, הוספת אלמנטים ואיתור ערכים.

```dart
void main() {
  // יצירת מפה
  var fruitColors = {
    'apple': 'red',
    'banana': 'yellow',
    'grape': 'purple'
  };

  // הוספת זוג מפתח-ערך חדש
  fruitColors['orange'] = 'orange';

  // גישה לערך לפי המפתח
  print(fruitColors['apple']); // פלט: red

  // עדכון ערך
  fruitColors['banana'] = 'green';

  // סיבוב על המפה
  fruitColors.forEach((fruit, color) {
    print('$fruit: $color');
  });
  // פלט לדוגמה:
  // apple: red
  // banana: green
  // grape: purple
  // orange: orange
}
```

עבור מבני נתונים מורכבים או פונקציונליות מורחבת, מתכנתי Dart לעיתים מסתמכים על ספריות נוספות. ספרייה כזו היא `collection` שמספקת סוגים מתקדמים של אוספים וכלים עזר. למרות ש-`collection` לא משנה את הדרך הבסיסית בה מטפלים ב-Maps, היא מעשירה אותם בפונקציות עזר וסוגים מתקדמים יותר של אוספים. הנה איך אפשר להשתמש בה עבור משימה ספציפית יותר, כמו מיון של מפה לפי הערכים שלה:

ראשית, וודא שהחבילה `collection` כלולה בקובץ `pubspec.yaml` שלך:

```yaml
dependencies:
  collection: ^1.15.0
```

לאחר מכן, ניתן להשתמש בה כך:

```dart
import 'package:collection/collection.dart';

void main() {
  var fruitColors = {
    'apple': 'red',
    'banana': 'yellow',
    'grape': 'purple',
    'orange': 'orange'
  };

  // מיון המפה לפי הערכים (צבעים)
  var sortedFruitsByColor = SplayTreeMap.from(
    fruitColors,
    (key1, key2) => fruitColors[key1]!.compareTo(fruitColors[key2]!)
  );

  print(sortedFruitsByColor);
  // פלט:
  // {orange: orange, apple: red, banana: yellow, grape: purple}
}
```

דוגמה זו מדגימה מיון של ערכי המפה לפי הערכים שלהם, ומציגה איך Dart והאקוסיסטם האקטיבי שלה יכולים לטפל במערכי אסוציאציות באופן זריז עבור מניפולציה מתקדמת של נתונים.

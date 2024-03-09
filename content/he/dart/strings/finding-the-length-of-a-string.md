---
title:                "מציאת אורך של מחרוזת"
date:                  2024-03-08T21:55:25.505535-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
מציאת אורך של מחרוזת (String) ב-Dart היא בעצם קביעת מספר היחידות קוד (למעשה, מספר התווים אם חושבים על זה בפשטות) במחרוזת נתונה. תכנתים עושים זאת כדי לתפעל מחרוזות עם דיוק גבוה יותר, כגון אימות קלט, קיצוץ טקסט להצגה, או עיבוד פורמטים שהאורך חשוב בהם (לדוגמה, פרוטוקולים עם הודעות שאורכן מוקדם).

## איך לעשות:
Dart מקל על קבלת אורך המחרוזת באמצעות המאפיין `length`. הנה דוגמה בסיסית:

```dart
void main() {
  String myString = "Hello, Dart!";
  print("The length of '\(myString)' is: \(myString.length)");
  // פלט: The length of 'Hello, Dart!' is: 12
}
```
המאפיין הזה סופר את מספר יחידות ה-UTF-16 במחרוזת, שמתאים לאורך המחרוזת לרוב השימושים הנפוצים.

לעיבוד טקסט מורכב יותר, במיוחד עם תווים ב-Unicode שנמצאים מחוץ ל-Plain Multilingual Basic (BMP), שקלו להשתמש בחבילת `characters` לספירת צברי גרפמים, שמייצגת באופן מדויק יותר את התווים כפי שהמשתמש תופס אותם.

ראשית, הוסיפו את `characters` ל-`pubspec.yaml` שלכם:

```yaml
dependencies:
  characters: ^1.2.0
```

ואז, השתמשו בזה כך:

```dart
import 'package:characters/characters.dart';

void main() {
  String myEmojiString = "👨‍👩‍👧‍👦 family";
  print("The length of '\(myEmojiString)' is: \(myEmojiString.characters.length)");
  // פלט: The length of '👨‍👩‍👧‍👦 family' is: 8
}
```

בדוגמה זו, `myEmojiString.characters.length` נותן לנו את האורך במונחים של צברי גרפמים של Unicode, שהוא ייצוג מדויק יותר למחרוזות שמכילות תווים מורכבים, כמו אמוג'ים או סימני תווים משולבים.

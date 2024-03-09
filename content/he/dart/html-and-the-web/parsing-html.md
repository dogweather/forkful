---
title:                "פיענוח HTML"
date:                  2024-03-08T21:55:28.688527-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
פיענוח HTML בתכנות כולל את חילוץ הנתונים ממסמכי HTML. תכניתנים עושים זאת על מנת להתקשר או לבצע סקרייפינג לתוכן אינטרנטי לצורך חילוץ מידע, בדיקות או אוטומציה, אפילו כאשר ממשקי API רשמיים אינם זמינים.

## איך לעשות:
Dart אינו מספק תמיכה מובנית לפיענוח HTML בספריות הליבה שלו. עם זאת, ניתן להשתמש בחבילה צד שלישי כמו `html` כדי לפרסר ולתפעל מסמכי HTML.

ראשית, הוסף את החבילה `html` לקובץ `pubspec.yaml` שלך:

```yaml
dependencies:
  html: ^0.15.0
```

לאחר מכן, יבא את החבילה לקובץ Dart שלך:

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

הנה דוגמה בסיסית לפיענוח מחרוזת המכילה HTML וחילוץ נתונים ממנה:

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>שלום, Dart!</h1>
      <p>זהו פסקה בדוגמת HTML</p>
    </body>
  </html>
  """;

  // פענוח המחרוזת HTML
  Document document = parse(htmlDocument);

  // חילוץ נתונים
  String title = document.querySelector('h1')?.text ?? "לא נמצא כותרת";
  String paragraph = document.querySelector('p')?.text ?? "לא נמצאה פסקה";

  print('כותרת: $title');
  print('פסקה: $paragraph');
}
```

פלט:

```
כותרת: שלום, Dart!
פסקה: זהו פסקה בדוגמת HTML
```

להתקשרות עם דפי אינטרנט מהעולם האמיתי, ייתכן שתשלב פיענוח `html` עם בקשות HTTP (באמצעות החבילה `http` לשליפת תוכן אינטרנטי). הנה דוגמה מהירה:

ראשית, הוסף את החבילות `http` יחד עם `html`:

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

לאחר מכן, תבצע שליפה ופיענוח של דף HTML מהאינטרנט:

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // שליפת הדף
  var response = await http.get(Uri.parse(url));
  
  if (response.statusCode == 200) {
    var document = parse(response.body);

    // נניח שהדף מכיל תגיות <h1> שמעניינות אותך
    var headlines = document.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('כותרות: $headlines');
  } else {
    print('הבקשה נכשלה עם סטטוס: ${response.statusCode}.');
  }
}
```

שים לב: טכניקת הסקרייפינג שהוצגה לעיל צריכה להיעשות באחריות ובהתאם לתנאי השירות של האתר.

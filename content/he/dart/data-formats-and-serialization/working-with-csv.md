---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:50.298045-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D0\u05EA \u05D6\
  \u05D4: \u05DB\u05D3\u05D9 \u05DC\u05D8\u05E4\u05DC \u05D1\u05E7\u05D1\u05E6\u05D9\
  \ CSV \u05D1-Dart, \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05EA\u05E2\u05D1\
  \u05D3\u05D5 \u05D0\u05EA \u05D4\u05D8\u05E7\u05E1\u05D8 \u05D1\u05D0\u05D5\u05E4\
  \u05DF \u05D9\u05D3\u05E0\u05D9 \u05D0\u05D5 \u05EA\u05E9\u05EA\u05DE\u05E9\u05D5\
  \ \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\
  \u05D9 \u05DC\u05E4\u05E9\u05D8 \u05D0\u05EA \u05D4\u05DE\u05E9\u05D9\u05DE\u05D4\
  . \u05DB\u05D0\u05DF \u05E0\u05E1\u05EA\u05DB\u05DC \u05E2\u05DC \u05E9\u05EA\u05D9\
  \ \u05D4\u05D2\u05D9\u05E9\u05D5\u05EA \u05D4\u05D0\u05DC\u05D5."
lastmod: '2024-04-05T21:53:40.160906-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05D8\u05E4\u05DC \u05D1\u05E7\u05D1\u05E6\u05D9\
  \ CSV \u05D1-Dart, \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05EA\u05E2\u05D1\
  \u05D3\u05D5 \u05D0\u05EA \u05D4\u05D8\u05E7\u05E1\u05D8 \u05D1\u05D0\u05D5\u05E4\
  \u05DF \u05D9\u05D3\u05E0\u05D9 \u05D0\u05D5 \u05EA\u05E9\u05EA\u05DE\u05E9\u05D5\
  \ \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\
  \u05D9 \u05DC\u05E4\u05E9\u05D8 \u05D0\u05EA \u05D4\u05DE\u05E9\u05D9\u05DE\u05D4\
  ."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D5\u05D1\u05E6\u05D9\
  \ CSV"
weight: 37
---

## איך לעשות את זה:
כדי לטפל בקבצי CSV ב-Dart, בדרך כלל תעבדו את הטקסט באופן ידני או תשתמשו בספריות צד שלישי לפשט את המשימה. כאן נסתכל על שתי הגישות האלו.

### ניתוח CSV באופן ידני
אם הצרכים שלכם פשוטים, ייתכן שתעדיפו לנתח באופן ידני מחרוזת CSV. ניתן להשיג זאת באמצעות פונקציות מניפולציה על מחרוזות היסוד של Dart:

```dart
void main() {
  // נתוני CSV לדוגמא
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // פיצול הנתוני CSV לשורות
  List<String> lines = csvData.split('\n');
  
  // ניתוח כל שורה
  List<Map<String, String>> data = [];
  List<String> headers = lines.first.split(',');
  
  for (var i = 1; i < lines.length; i++) {
    List<String> row = lines[i].split(',');
    Map<String, String> record = {};
    for (var j = 0; j < headers.length; j++) {
      record[headers[j]] = row[j];
    }
    data.add(record);
  }
  
  // פלט של הנתונים שננתחו
  print(data);
}

// פלט לדוגמא:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

### שימוש בספריית צד שלישי: `csv`
לסצנריות מורכבות יותר או כדי לפשט את הקוד שלכם, תוכלו להשתמש בספריית צד שלישי פופולרית כמו `csv`. ראשית, הוסיפו אותה לפרויקט שלכם על ידי כלול `csv: ^5.0.0` (או הגרסה האחרונה) בקובץ ה-`pubspec.yaml` שלכם תחת `dependencies`. לאחר מכן השתמשו בה כך:

```dart
import 'package:csv/csv.dart';

void main() {
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // שימוש ב-CsvToListConverter לניתוח הנתוני CSV
  List<List<dynamic>> listData = const CsvToListConverter().convert(csvData);
  
  // הפריט הראשון ברשימה מכיל את הכותרות
  List<String> headers = listData.first.map((item) => item.toString()).toList();
  
  // הסרת שורת הכותרת לפני המשך העיבוד
  listData.removeAt(0);
  
  // המרה ל-List<Map<String, dynamic>> לפורמט מובנה יותר
  List<Map<String, dynamic>> mappedData = listData.map((list) {
    Map<String, dynamic> map = {};
    for (int i = 0; i < headers.length; i++) {
      map[headers[i]] = list[i];
    }
    return map;
  }).toList();
  
  // פלט של הנתונים שהופכו
  print(mappedData);
}

// פלט לדוגמא:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

שתי השיטות מדגימות איך לעבוד עם נתוני CSV: הראשונה באופן ידני, למטרות למידה או כאשר מתמודדים עם מבני CSV פשוטים מאוד; השנייה, על ידי ניצול ספרייה חזקה שמפשטת את הניתוח ויכולה להתמודד עם מגוון רחב של מורכבויות בעיצוב CSV.

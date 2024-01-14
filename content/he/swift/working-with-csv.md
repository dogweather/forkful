---
title:                "Swift: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## למה

התכנות בשפת Swift יכול להיות מאתגר ומתעסק עם נתונים יכול להיות דבר מסובך. אבל, כניסה לנתוני CSV יכולה לסייע לך לנהל נתונים בפורמט שונה ולעזור לך לעבוד בקלות עם נתונים בתוך סביבות מגוונות. באמצעות שימוש בשפת Swift לעבודה עם נתוני CSV, אתה יכול להיות יעיל יותר בניהול הנתונים שלך ולהקל על התכנות בכלל.

## איך לעבוד עם CSV בשפת Swift

בשימוש בשפת Swift, ניתן לקרוא קובץ CSV בעזרת מחלקת `String`. יש להשתמש בפקודות `do` ו- `try` לקריאת הקובץ ואז להוציא את הנתונים שבו לעזרת הפונקציות של `String` כמו `components(separatedBy:)`. הנה דוגמא של קוד ופלט:

```Swift
do {
  let csvString = try String(contentsOfFile: "data.csv", encoding: .utf8)
  let rows = csvString.components(separatedBy: "\n")
  for row in rows {
    let columns = row.components(separatedBy: ",")
    print(columns)
  }
} catch {
  print("Error reading CSV file: \(error)")
}
```

Output:
```
["Name", "Age", "Hobby"]
["John", "25", "Hiking"]
["Sarah", "30", "Reading"]
```

## צעדים נוספים לעומק יותר

כדי להיות יעיל יותר בעבודה עם נתוני CSV בשפת Swift, כדאי להשתמש במחלקת `CSVDecoder` הכפולה מ-`Codable`. יש ליצור מודל שמתאים לנתונים שיש בקובץ CSV, ואז ניתן להשתמש במחלקת `CSVDecoder` על מנת לקרוא את הנתונים ולהמירם למודל המתאים. הנה דוגמא לקוד ופלט:

```Swift
struct Person: Codable {
  let name: String
  let age: Int
  let hobby: String
}

do {
  let csvString = try String(contentsOfFile: "data.csv", encoding: .utf8)
  let decoder = CSVDecoder()
  let people = try decoder.decode([Person].self, from: csvString, withHeader: true)
  print(people)
} catch {
  print("Error decoding CSV: \(error)")
}
```

Output:
```
[Person(name: "John", age: 25, hobby: "Hiking"), Person(name: "Sarah", age: 30, hobby: "Reading")]
```

## ראי גם

למידע נוסף על עבודה ע
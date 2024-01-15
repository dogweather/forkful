---
title:                "עבודה עם ימל (Yaml)"
html_title:           "Swift: עבודה עם ימל (Yaml)"
simple_title:         "עבודה עם ימל (Yaml)"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה

אם אתה מתעסק עם שפת תכנות כמו Swift, ייתכן שתגיע לקרוא על YAML. אז למה כדאי ללמוד ולהתחיל לעבוד עם YAML? כי זו שפה פשוטה וקלה לשימוש שתמצא אצלה תחביר ברור ונוח לזיכרון והיא נפוצה בסביבת פיתוח.

## איך לעבוד עם YAML

היי כולם, היום אני אראה לכם כיצד לעבוד עם YAML בשפת תכנות Swift. כדי להתחיל, נצטרך להתקין את ספריית YAML באמצעות הוראת ההתקנה המתאימה לתוכנית שלנו. לדוגמה, אם אנו משתמשים במנהל החבילות של Swift, נוסיף את השורה הבאה לקובץ Package.swift:

```Swift
dependencies: [
    .package(url: "https://github.com/behrang/YamlSwift.git", from: "3.4.0")
]
```
לאחר מכן, נוכל להתחיל לעבוד עם YAML בקלות על ידי ייבוא הספריה ושימוש בפקודות המתאימות. נראה לדוגמה איך ניתן לקרוא את הנתונים מתוך קובץ YAML:

```Swift
import Yaml

let yamlString = """
name: John Smith
age: 30
"


let yamlData = try Yaml.load(yamlString)

// מודפס הופעה כאשר הקוד רץ: John Smith, 30
print(yamlData["name"].string, yamlData["age"].int)
```

כמו בדוגמה למעלה, את הנתונים שלנו אנו קוראים עם עזרת הפקודות של הספריה ומבצעים עליהם את הפעולות הרצויות. כמובן שניתן גם לכתוב וליצור קובץ YAML באמצעות הספריה ולשמור אותו על המחשב שלנו, כך שנוכל להשתמש בו בצורה נוחה ופשוטה.

## טיפים נוספים לעבודה עם YAML

- YAML מאפשרת לנו לשנות ולעדכן את הנתונים בקובץ מקורי ולהשתמש בפקודות כגון add, delete ו-update כדי לקבל את התוצאות הרצויות.
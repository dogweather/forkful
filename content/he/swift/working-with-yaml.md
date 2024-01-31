---
title:                "עבודה עם YAML"
date:                  2024-01-19
html_title:           "Bash: עבודה עם YAML"
simple_title:         "עבודה עם YAML"

category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה ולמה? עבודה עם YAML בשפת Swift מאפשרת לקרוא ולכתוב קבצי YAML, תסדיר נפוץ להגדרות תצורה. תכניתנים עושים זאת משום שזה פשוט לקריאה ונוח לשיתוף קונפיגורציות.

## How to:
איך לעשות:
קודם, ידרש מודול חיצוני כי Swift לא מספק פונקציונליות קריאת YAML ישירות. נוכל להשתמש ב-Yams, למשל. תקינו דרך Swift Package Manager (SPM). אחרי הגדרת התלות, ניתן לקרוא ולכתוב YAML כך:

```Swift
import Yams

// קריאת YAML
let yamlString = """
name: Yosef
job: Developer
skills:
  - Swift
  - SwiftUI
"""

if let person = try? Yams.load(yaml: yamlString) {
    print(person)
}

// כתיבת YAML
let personDict: [String: Any] = [
  "name": "Yosef",
  "job": "Developer",
  "skills": ["Swift", "SwiftUI"]
]

if let yamlOutput = try? Yams.dump(object: personDict) {
    print(yamlOutput)
}
```

פלט לדוגמא:
```
// פלט קריאת YAML
["name": Yosef, "job": Developer, "skills": [Swift, SwiftUI]]

// פלט כתיבת YAML
name: Yosef
job: Developer
skills:
  - Swift
  - SwiftUI
```

## Deep Dive
טבילה עמוקה:
YAML ("YAML Ain't Markup Language") מקורו משנת 2001, כאלטרנטיבה ל-XML ול-TOML. תכונותיו של שפת הסימונים הזו כוללות אנושיות ונוחות עיבוד על ידי מחשב. ב-Swift, חוסר היכולת לקרוא ישירות YAML דורש שימוש בספריות כמו Yams. Yams מספקת API שמאפשר לנו לעבוד עם YAML באופן ידידותי ל-Swift.

## See Also
ראה גם:
- דוקומנטציה של Yams בגיטהאב: [Yams GitHub](https://github.com/jpsim/Yams)
- מורה ל-YAML: [Learn YAML in Y minutes](https://learnxinyminutes.com/docs/yaml/)
- דוקומנטציה של YAML: [The Official YAML Website](https://yaml.org)

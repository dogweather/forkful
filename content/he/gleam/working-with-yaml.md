---
title:                "עבודה עם YAML"
html_title:           "Bash: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
YAML הוא תבנית קלה להכנה ולקריאה של נתונים, בעיקר להגדרות תצורה ושימושים דומים. תוכניתנים משתמשים בו כי הוא נוח לקריאה ועריכה גם עבור בני אדם וגם עבור מחשבים.

## איך לעשות:
כרגע Gleam אינו תומך בעיבוד YAML באופן פנימי בספרייה התקנית. עלייך להשתמש בגורמים חיצוניים כמו קריאות למערכת או חיבור ספריית Rust לעיבוד YAML. לדוגמה:

```gleam
// עדכן את הקוד לרגע שיתווסף תמיכה בעיבוד YAML או המשך להשתמש בגישות חיצוניות
```

## צלילה עמוקה:
YAML, שמציין "YAML Ain't Markup Language", פותח במטרה להיות האמצעי הפשוט ביותר לתיאור תצורות. הוא שימש כחלופה לXML ועכשיו ניתן לראות שהוא מתחרה גם בJSON בקרב המהנדסים. ב-Gleam, אם תרצה לעבוד עם YAML יהיה עליך לשלב קוד משפות אחרות שתומכות בכך, מאחר ו-Gleam מותאם באופן מושלם לקוד חיצוני ובעיקר קוד בRust ובElixir.

## ראה גם:
- תיעוד ה-Gleam עבור [שילוב קוד חיצוני](https://gleam.run/book/tour/external-functions.html)
- תקן [YAML](https://yaml.org/spec/1.2/spec.html)
- פרויקט [serde_yaml](https://github.com/dtolnay/serde-yaml) לעיבוד YAML ב-Rust
- מדריך [YAML](https://learnxinyminutes.com/docs/yaml/) באתר Learn X in Y minutes

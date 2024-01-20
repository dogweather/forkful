---
title:                "עבודה עם yaml"
html_title:           "Elm: עבודה עם yaml"
simple_title:         "עבודה עם yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם YAML היא דרך נוחה ויעילה להיכנס לתקשורת בין שפות תכנות שונות. תוכניות מסוימות משתמשות בפורמט זה כדי להתאים בין פלטפורמות שונות או לתכנן תצוגות שונות לאזורים שונים. זה נפוץ במיוחד ב JavaScript והשתתפות ו הפרימטיבים.

## איך לעשות?

כדי לעבוד עם YAML ב־ Elm, ניתן להשתמש בחבילת קוד פתוח YAML שנקראת [`urtela/elm-yaml`](https://package.elm-lang.org/packages/urtela/elm-yaml/latest/). כדי להתחיל, נדרוש לייבא את החבילה:

```elm
import YAML exposing (..)
```

כעת, ניתן להשתמש בפונקציות כמו `decode` לקריאת תצורת YAML שמכילה תבניות שונות והמרתה לפורמט מבנים.

```elm
decode """
  language: Elm
  version: "0.19"
  date_created: "2020-01-01"
"""
```

פלט:

```elm
Ok (Dict.fromList [("language", "Elm"), ("version", "0.19"), ("date_created", "2020-01-01")])
```

בנוסף, ניתן להשתמש בפונקציות נוספות כמו `encode` להמרת פורמט מבנים לתצורת YAML, וגם לקרוא ולכתוב קבצי YAML חיצוניים.

## חפירה עמוקה

היסטורית מקור פורמט ה־YAML תחתום בשנת 2001 על ידי Clark Evans. פורמט זה תוכנן כדי להמיר פורמט אחר בשם `YAML Ain't Markup Language`. ישנן אלטרנטיבות אחרות לפורמט זה כמו JSON ו־ XML, אך YAML נחשב לפשוט יותר לקריאה ולכתיבה וכן קל יותר להבין מאשר פורמטים אחרים.

כדי להשתמש בחבילת `elm-yaml`, נדרש להתקין את הגרסה המתאימה ל־ Elm. ניתן למצוא את החבילה וגם דוגמאות נוספות בכתובת [`https://package.elm-lang.org/packages/urtela/elm-yaml/latest/`](https://package.elm-lang.org/packages/urtela/elm-yaml/latest/).

## ראה גם

* [`https://www.yaml.info/`](https://www.yaml.info/) - אתר YAML הרשמי עם מידע נוסף ורשימת כל החבילות הקיימות לשפות תכנות שונות.
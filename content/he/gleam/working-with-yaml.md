---
title:                "Gleam: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

# מדוע

הכתיבה עם YAML יכולה להיראות מסובכת למתכנתים מתחילים, אבל לפעמים זה האופן הטוב ביותר לאחסון מידע מבני ומסוגנן. השתמשות ב YAML תכנס לידיים מאוד בעזרת גלים, שפת תכנות איכותית ומודולרית. 

# איך לעבוד עם YAML בגלים

הנה דוגמאות של שימוש ב YAML עם גלים שיכולות לעזור לך להתחיל.

```gleam
import yaml

let data = yaml.decode("קובץ.yml")
```

כעת, אתה יכול לעבוד על המידע שיקראת מתוך קובץ YAML כמו שאתה יכול עם כל מבנה נתונים בגלים. כאן תוכל למצוא מידע על עבודה עם עצים, לוקטקים וטיפוסים התאמה לחלום של גלים (https://gleam.run/cats/possible-dream.html).

```gleam
fn print_some_data(data) {
  case data {
    Value.Null -> Err("לא מצאנו את המידע, נסה שוב")
    Value.Bool(b) -> Ok(if b { "כן" } else { "לא" })
    Value.Float(f) -> Ok(Float.to_string(f))
    Value.Integer(i) -> Ok(Integer.to_string(i))
    Value.String(s) -> Ok(s)
    Value.List(list) ->
      list
      |> List.map(print_some_data)
      |> List.foldl(|n, xs| Ok(n ++ ", " ++ xs), Ok(""))
      |> Result.unwrap_or("אין מידע ברשימה")
    Value.Map(map) ->
      map
      |> Dict.to_list
      |> List.map(print_some_data)
      |> List.map(Result.to_option)
      |> List.filter(Option.is_some)
      |> List.map(Option.unwrap)
      |> List.foldl(|n, xs| Ok(n ++ ", " ++ xs), Ok(""))
      |> Result.unwrap_or("אין מידע במיפה")
  }
}

print_some_data(data)
```

כשהקוד מסתיים, המידע המבוסס על YAML יהיה בפורמט שנוח יותר לעבוד איתו על ידי המתכנתים.

# יישום מעמיק בעבודה עם YAML

בגלים, ישנם מודולים נוספים שיכולים לסייע בעבודה עם YAML יותר ממה שיכול להיות די מוכר בפנייך. נהדס כי ניתן לפענח ישירות מתוך טכניקת המידע YAML שלך על ידי השתמשות במודול node-hop (https://hop.jumpstart.sh). נהדר הייצוא של מידע YAML בגיה
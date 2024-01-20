---
title:                "שימוש בביטויים רגולריים"
html_title:           "Bash: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
רגולר אקספרשנים (Regular Expressions) זה צורת חיפוש והחלפה מתקדמת בטקסט. תוכניתנים משתמשים בזה כדי לסנן, למלא, ולנתח נתונים ביעילות.

## איך לעשות:
```gleam
import gleam/regex

fn run_regex_demo() {
  let pattern = regex.regex("ב(ר|ל)וך (ה|ש)ב(א|ע)")
  let text = "ברוך הבא לגלים!"
  
  case regex.find(pattern, text) {
    Ok(result) -> result
    Error(_) -> "לא נמצאה התאמה."
  }
}

fn main() {
  run_regex_demo()
  |> io.debug
}
```

תוצאה אפשרית:
```
"ברוך הבא"
```

## עיון עמוק
רגולר אקספרשנים תחילה הוצגו בשנות ה-50 ומאז שיפרו מאוד. קיימות חלופות כמו פארסינג באמצעות גרמטיקות, אך לאקספרשנים יתרונות במהירות ונוחות. בגלים, `gleam/regex` מאפשר ליצור ולהחיל ביטויים רגולריים בקוד.

## גם ראה
- תיעוד Gleam `gleam/regex`: [https://hexdocs.pm/gleam_stdlib/gleam/regex/](https://hexdocs.pm/gleam_stdlib/gleam/regex/)
- מבוא לרגולר אקספרשנים: [https://regexone.com/](https://regexone.com/)
- MDN Web Docs על רגולר אקספרשנים: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
---
title:                "קריאת ארגומנטים מפקודת השורת פקודה"
html_title:           "Gleam: קריאת ארגומנטים מפקודת השורת פקודה"
simple_title:         "קריאת ארגומנטים מפקודת השורת פקודה"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# מה ולמה?
קבלת ארגומנטים מפקודת השורת פקודה היא תהליך בו המחשב מקבל מידע מסוים מהמשתמש דרך הפקודה. תהליך זה חשוב לפותחים מכיוון שהוא מאפשר להתאים את תכניותיהם לצרכי המשתמש ולהפעיל אותן באופן יעיל יותר.

# איך לעשות את זה:
```Gleam
fn main(args: List(String)) {
  io.println("The given arguments are:")
  Gleam.List.map(io.println, args)
}  
```
הקוד הנ"ל מדפיס את הארגומנטים שהועברו לתכנית דרך הפקודת השורת פקודה באמצעות גרסה מפורמטת הקובץ גלים. כאשר תיכנס לארגומנטים לתכנית, היא תדפיס את המחרוזת "The given arguments are:" ואת כל הארגומנטים בנפרד.

# טיפול עמוק:
קבלת ארגומנטים מהשורת פקודה הוא תהליך ידוע בעולם התכנות ומשמש לא מעט בתכניות שונות. תהליך זה עזר לפתח תכניות יותר גמישות ומותאמות למשתמש. תהליך זה נמצא בשימוש רב בימינו ומאפשר למשתמשים לשלוט על התוכניות שהם משתמשים בהן.

# ראה גם:
למידע נוסף על קבלת ארגומנטים מהשורת פקודה ושימושים נוספים, ניתן לעיין במקורות המקושרים להלן:
- [תיעוד של גרסת גלים הנוכחית](https://gleam.run/documentation/guide/tooling/#command-line-arguments)
- [ספר המוקדש לתוכנית Gleam](https://gleam.run/book/tour/command-line-arguments)
---
title:                "קריאת ארגומנטים בקווים הפקודה"
html_title:           "Gleam: קריאת ארגומנטים בקווים הפקודה"
simple_title:         "קריאת ארגומנטים בקווים הפקודה"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## למה

השתמשו באגרונומיה כמו המומחה שאתם עבור. קריאת פרמטרי שורת הפקודה יכולה להיות כלי עוצמתי כאשר מדובר על פיתוח עם לינקס או במערכות הפעלה הרלוונטיות שלכם. תוכלו להשתמש בכלי זה כדי לאפשר למשתמשים להתאים את התוכנה שלכם לצרכים שלהם באופן פשוט יותר.

## איך לעשות זאת

התחילו על ידי יצירת אובייקט חדש עבור פרמטרי שורת הפקודה.

```Gleam
let args = @@os.get_arguments()
```

כעת, תוכלו להשתמש בקוד זה כדי לקרוא ולעבד את הפרמטרים שיועברו כארגומנטים לתוכנית שלכם. לדוגמה, נניח שרצינו להדפיס את כל הפרמטרים שאנו מקבלים:

```Gleam
args
|> List.map(print)
```

פלט:
```
> ./my-program --name "John Doe" -age=30
./my-program
--name
"John Doe"
-age=30
```

תוכלו גם להשתמש בפרמטרים כדי להגדיר פעולות נוספות של תוכניתכם. למשל, נניח שאנו רוצים לבדוק אם מועבר פרמטר "help" כדי להדפיס הוראות למשתמשים:

```Gleam
args
|> List.filter(~match(~help, arg) -> true
                ; _ -> false)
|> do
    |> if List.is_empty, do: show_help(),
       else: args
```

##ייעוד

כעת שהינכם יודעים איך לקרוא פרמטרי שורת הפקודה ב-Gleam, תוכלו להתעמק עוד יותר ולחקור תכונות נוספות כמו יישומי כוח של פרמטרים וקשיות עם פרמטרים רבים.

##ראו גם

- [Reading Command-Line Arguments in Gleam](https://gleam.run/articles/reading-command-line-arguments/)
- [Gleam Official Documentation](https://gleam.run/documentation/)
- [Gleam NPM Package](https://npmjs.com/package/gleam-cli)
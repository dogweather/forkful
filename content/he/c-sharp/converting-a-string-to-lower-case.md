---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ?מה ולמה

המרת מחרוזת לאותיות קטנות היא כאשר אנחנו משנים את כל האותיות במחרוזת לגרסאות קטנות שלהן. זה זמן רב כאשר אנחנו מניחים שמופעוני מילא מעניקים משמעות שונה - "pwd" לעומת "PWD", למשל.

## כיצד:

הנה קוד פשוט שמראה איך להמיר מחרוזת לאותיות קטנות בשפת C#:

```C#
string myString = "HELLO WORLD";
string lowerCaseString = myString.ToLower();
Console.WriteLine(lowerCaseString);  // Prints: hello world
```

## צלילה עמוקה

אף על פי שהפונקציה ToLower() הכי פשוטה ומסחרית, ישנם שיקולים נוספים שצריך לקחת בחשבון כאשר מדובר על המרת מחרוזות לאותיות קטנות.

1. היסטורית - מההתחלה של תכנות מחשבים, הפעלה של תווים הייתה נמשכת בעמידה אוניברסלית זמן רב. זה הוביל לצורך בפונקציות, כמו ToLower(), שקלות להבנה והשתמשו בהן לעבר העקומת למידה.

2. חלופות - דרך אחרת להתמודד עם אין התאמה בין אותיות גדולות וקטנות היא באמצעות אופציה של "case-insensitive" שלא מתמקדת באותיות קטנות או אותיות ראשית במחרוזות.

3. פרטים של היישום - זכרו שהפונקציה ToLower() לא בהכרח עובדת עם תווים שאינם אותיות או אותיות שאינן באלפבית הלטיני. אם מחרוזת שלכם מכילה תווים כאלה, יתכן שתצטרכו להשתמש בפונקציה שמטפלת באופן специфי בתווים שלא ממומשים בפונקציה ToLower().

## עיון נוסף

את קוד הפונקציה ToLower() ניתן למצוא ב-:

- דוקומנטציה של Microsoft C# תיעוד: https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower
- "C# Station" מבטל את המרת גדולות עם פונקציות ושיטות נוספות: https://csharp-station.com/Tutorial/CSharp/lesson07
- אתר האינטרנט של "Dot Net Perls", שהם שולחים למבט עמוק יותר לעבודה עם מחרוזות ב- C#: https://www.dotnetperls.com/string
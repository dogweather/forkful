---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "C#: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה
כדי לשפר את חוויית המשתמש והקריאות בקוד, ייתכן שנרצה להמיר מחרוזת או כתב לאותיות קטנות במקרים שונים.

## איך לעשות זאת
המירה לאותיות קטנות במחרוזת נעשית באמצעות שימוש בפעולת "ToLower" שבתוך המחלקה "String", כך:

```csharp
string myString = "HELLO WORLD";
string lowerCaseString = myString.ToLower();
Console.WriteLine(lowerCaseString);

// Output: hello world
```
בדוגמא הזו, אנו משתמשים במחרוזת המכילה אותיות גדולות ומבצעים עליה את הפעולה "ToLower" כדי להמיר את האותיות לקטנות. עם זאת, יש לציין שפעולה זו אינה משנה את המחרוזת המקורית, אלא שמייצרת מחרוזת חדשה המכילה את האותיות הקטנות.

## חקירה מעמיקה
כדי להבין טוב יותר את תהליך המירה לאותיות קטנות, נוכל להכיר כמה מאפיינים עמוקים של הפעולה "ToLower". ראשית, ניתן להשתמש בפרמטרים נוספים כדי להגדיר את התרגום של אותיות גדולות מיוחדות, כך:

```csharp
string myString = "HÉLLO";
string lowerCaseString = myString.ToLower(new CultureInfo("fr-FR"));
Console.WriteLine(lowerCaseString);

// Output: héllo
```

בדוגמא זו, השתמשנו בפרמטר של CultureInfo כדי לציין את השפה של התרגום. שימו לב כיצד האותיות המיוחדות נמצאות באותו התרגום כמו האותיות הרגילות.

נוסף לכך, הפעולה "ToLower" יכולה לקבל גם פרמטר שמיישר את התרגום לפי אותיות מקובצות באלפבית. בדוגמא הקודמת, נוכל להשתמש בפרמטר הבודד במקום להשתמש ב-CultureInfo.

## ראו גם
כדי לקבל עוד מידע על פעולות כמו המירה לאותיות קטנות, ניתן לעיין במ
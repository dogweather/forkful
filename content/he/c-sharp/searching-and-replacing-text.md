---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?

חיפוש והחלפה של טקסט הם הליך שבו מסננים טקסט בעזרת תנאי מסוים (חיפוש) ואז מחליפים אותו דרך תנאי/דינמיקה אחרת (החלפה). תכנתים משתמשים בזה בעיקר כדי לשנות, לנקות, או למסדר קובץ של טקסט.

## כיצד: 

C# מספקת את הפונקצייה `Replace()` של המחלקה `String` להחלפת טקסט. 
```C#
string sentence = "אני למדתי פייתון";
string newSentence = sentence.Replace("פייתון", "C#");

Console.WriteLine(newSentence); // אני למדתי C#
```
עוד אפשרות היא להשתמש בביטויים רגילים כדי להחליף טקסט, במיוחד כאשר החוקים מורכבים מאוד.
```C#
string sentence = "אני למדתי פייתון";
string newSentence = Regex.Replace(sentence, "פייתון", "C#");

Console.WriteLine(newSentence); // אני למדתי C#
```

## עומק:

1. **היסטוריה:** חיפוש והחלפת טקסט היו הכרחיים מאז תחילת התכנות. השיטות נעשה שימוש באופן רחב יותר עם התפשטות שלנוערות טקסט.
2. **אלטרנטיבות:** קיימות שיטות אחרות לחיפוש והחלפה של טקסט, כמו שימוש במתודות מותאמות אישית שמשתמשות בלולאות, אך `Replace()` היא השיטה הפשוטה ביותר.
3. **נתונים אימפלמנטציה:** `Replace()` של `String` בC# מחזירה את המחרוזת החדשה ולא משנה את המחרוזת המקורית. זה מכיוון שהמחרוזות בשפת C# הן Immutable.

## קישורים מועילים:

1. [מדריך Microsoft על פונקציית Replace()](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-5.0)
2. [טיפים לשימוש בביטויים רגילים בC#](https://www.dotnetperls.com/regex)
3. [פרק מחברת קורס Learn-C#.org על מחרוזות](https://www.learn-c#.org/strings/)
---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה זה ולמה? | What & Why?
מחיקת תווים התואמים לתבנית הוא שיטה שבה מוחקים תווים מחרוזת או ממערך של תווים המתאימים לתבנית מסוימת. התכנתים בדרך כלל משתמשים בשיטה זו כאשר הם צריכים לנקות, לתקן או להוציא מידע מנתונים מסוימים.

## איך מבצעים זאת? | How To
כאן, אנו מביאים שני דרכים למחוק תווים שתואמים לתבנית.

```C#
// Using Built-in String Replace method
string sampleText = "123abc456def";
string pattern = "abc";
string newText = sampleText.Replace(pattern, "");
Console.WriteLine(newText); // Output: 123456def

// Using Regular Expressions
using System.Text.RegularExpressions;

string sampleText2 = "123abc456def";
Regex rgx = new Regex("abc");
string newText2 = rgx.Replace(sampleText2, "");
Console.WriteLine(newText2); // Output: 123456def
```

## צלילה עמוקה | Deep Dive
מחיקת תווים התואמים לתבנית היא טכניקה שנמצאת בשימוש מורחב מאז הימים הראשונים של התכנות. במהלך השנים, שיפרו שפות תכנות רבות את דרכיהן של מידוע תווים תואמים לתבניות מקצועיות.
השיפור הגיע עם שפת התכנות C# כאשר הוצגה הפונקציה Replace של מחרוזות ועיבוד תווים ומחרוזות באמצעות ביטויים רגילים.  
כלול מחליפה עבודה הכי טוב כאשר אתה יודע מראש איזה תבנית אתה רוצה למחוק. ביטויים רגילים הם כלי עוצמתי וגמיש הרבה יותר, שאפשר לאתר ולמחוק תבניות מורכבות יותר במחרוזות.

## עיין גם | See Also
1. [Replace Method - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-5.0)
2. [Regular Expressions - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
3. [C# Language Guide - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/csharp/)
---
title:                "C#: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

המטרה של חיפוש והחלפה של טקסט היא לאפשר למתכנתים לשנות את הטקסט בקלות ולאחר מכן לשחזר אותו תוך שמירה על קוד נקי ומאורגן. זה יכול להוביל להפחתת זמן פיתוח ולשיפור היציבות של התוכנה.

כיצד לבצע חיפוש והחלפה בקוד של C#: 

```C#
//שימוש בתוכנית Regex לחיפוש והחלפת טקסט 
using System.Text.RegularExpressions;

//שימוש בשיטה Replace על מחרוזת כדי להחליף טקסט
string newString = Regex.Replace(originalString, "textToReplace", "replacementText");

//דוגמה:
string original = "זהו טקסט לדוגמה";
string newString = Regex.Replace(original, "טקסט", "מתמטיקה");

//output: זהו מתמטיקה לדוגמה
```

עומקנו:

כמו כל כלי חזק, ישנם עוד כמה דברים שניתן לעשות עם חיפוש והחלפה של טקסט. לדוגמה, ניתן להשתמש בתכונות נוספות של Regex כגון ניתוח הביטויים הרגולריים, שינוי גודל האותיות, ועוד. בנוסף, כדאי להבצע חיפוש והחלפה על מחרוזות גדולות על מנת לתקן שגיאות כמו תווים ריקים או תווים ספציפיים שגויים.

על מנת ללמוד עוד על חיפוש והחלפה של טקסט ב-C#, ניתן לבדוק את המדריכים הבאים:

- [Microsoft Docs: Regex Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=netframework-4.8)
- [C# Regex Tutorial](https://www.c-sharpcorner.com/UploadFile/dbd951/regular-expression-in-C-Sharp/)
- [Codecademy: Learn to use Regular Expressions in C#](https://www.codecademy.com/learn/learn-regular-expressions/modules/learn-regular-expressions-csharp/) 

ראה גם:

- [Markdown: שיטת כתיבה פשוטה לכתיבת מדריכים טכניים](https://daringfireball.net/projects/markdown/)
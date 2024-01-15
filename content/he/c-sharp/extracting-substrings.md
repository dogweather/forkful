---
title:                "חילוץ תת-מחרוזות"
html_title:           "C#: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

# למה

אז למה אנשים היו רוצים להתעסק בחילוץ מחרוזות מתוך מחרוזת אב גדולה? בעיקר, זה עשוי להיות נחוץ כאשר יש לנו מחרוזת גדולה ואנחנו רוצים להשתמש רק בחלק ממנה, למשל כאשר אנחנו רוצים למצוא מילה מסוימת במחרוזת ארוכה יותר ולהחליף אותה במילה אחרת.

# איך לעשות זאת

למדנו כבר את כמה דרכים להשתמש במחרוזת `Substring ()` בקטע הקוד הקודם. אבל ניתן לעשות דברים נוספים כדי למצוא מחרוזות מתוך מחרוזת אב גדולה. הנה כמה דוגמאות נוספות שתוכלו לנסות בעצמכם:

```C#
string largeString = "זה מחרוזת גדולה!";
string smallSubstring = largeString.Substring(4); // תחילת המחרוזת תתחיל מהמיקום הרביעי
Console.WriteLine (smallSubstring); // ידפיס "מחרוזת גדולה!"

string smallSubstring2 = largeString.Substring(4, 4); // תחליף את האותיות במיקום הרביעי עד השמיני
Console.WriteLine (smallSubstring2); // ידפיס "מחרה"

int index = largeString.IndexOf("ה"); // יחזיר את המיקום של האות "ה" במחרוזת
string substringFromIndex = largeString.Substring(index); // תחליף את המחרוזות מהמחקר החדש
Console.WriteLine (substringFromIndex); // ידפיס "המחרוזות גדולה!"

```

# יצירת נימוק נמוך

העולם של המחרוזות הוא נרחב ומעניין וניתן לעשות עוד המון דברים מעניינים כאשר משתמשים בנמוק המתחזק של `Substring ()`. למרבה המזל, ישנם כלים נוספים כמו `Split ()` ו- `Join ()` שיכולים לעזור לנו לעבוד עם מחרוזות. כמו כן, ישנם גם פונקציות נוספות כמו `StartsWith()` ו- `EndsWith()` שיכולות להיות מועילות בהתמודדות עם מחרוזות.

# רכיב עמוק

הנה עוד כמה ד
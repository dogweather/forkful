---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
אורך המחרוזת הוא מספר התווים שבה. תכנתים אותו כדי לאמור משימות כמו לבדוק תקינות, לשלב מחרוזות או לצייר שאלות בפלט.

## איך ל:
ההצעה הכי פשוטה היא להשתמש במאפיין Length של מחרוזת. למשל:
```PowerShell
$string = "עולם הפאוורשל"
$string.Length
```
הפלט של קוד זה יהיה 12, כי יש 12 תווים במחרוזת.

## צלילה עמוקה
Historically, דרך מקובלת יותר לקבל את אורך מחרוזת ב־PowerShell היא באמצעות הפעולה Length. 

מחליפים נוספים כוללים שימוש ב- `[char[]]` להפוך את המחרוזת למערך של תווים, ואז למדוד אורך של מערך, או בהמרה את המחרוזת למערך של bytes באמצעות `[System.Text.Encoding]::UTF8.GetBytes()` ולמדוד את אורך של המערך.

הכוח של PowerShell נבנה על הידע של .NET ואתה יכול להשתמש בדרך זו. אבל בדרך כלל, המאפיין `.Length` הוא דרך מהירה ופשוטה למדוד את אורך של מחרוזת.

## ראה גם
- `[MSDN: String.Length]` (https://msdn.microsoft.com/en-us/library/system.string.length.aspx)
- `[StackOverflow: Ways to find the length of a string in PowerShell]` (https://stackoverflow.com/questions/12697259/how-to-count-the-char-in-a-string-using-powershell)
---
title:                "חיבור מחרוזות"
html_title:           "C++: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיבור מחרוזות הוא התהליך שבו משרשרים שתיים או יותר מחרוזות לך ליצור מחרוזת אחת ארוכה יותר. מתכנתים עושים זאת לדעיך, להפשט קוד וליצור נתונים במידה הדרושה. 

## כיצד:
חיבור מחרוזות ב-PowerShell ניתן לעשייה בשלושה דרכים עיקריות. הבא הוא דוגמת קוד ב-PowerShell נגיחה לשלוש השיטות:

```PowerShell
# שיטה 1: חיבור עם סימן המחבר "+"
$str1 = "שלום, "
$str2 = "עולם!"
Write-Host ($str1 + $str2) # הפלט: שלום, עולם!

# שיטה 2: חיבור עם הפקודה "Concat"
$str3 = [string]::Concat($str1, $str2)
Write-Host $str3 # הפלט: שלום, עולם!

# שיטה 3: שימוש במתודת "f" 
$str4 = "{0}{1}" -f $str1,$str2
Write-Host $str4 # הפלט: שלום, עולם!
```

## הצצה לתוך:
חיבור מחרוזות הוא מעט מאוד ראש-צעיר קיים בלשון תכנות PowerShell, אך בסביבות תכנות אחרות. זה נתמך ב-PowerShell מאז הגרסה הראשונה. היינו חלב מהכולל נוח להשתמש, שרתים טמבל ויציב, אך קיימות חלופות אחרות שיכולות לעזור במתנאים מסוימים, כמו `StringBuilder` שיכול לעזור כאשר יש צורך בביצועים גבוהים עם מחרוזות גדולות.

## ראה גם:
שקוף כאן למידע נוסף על מחרוזות ב-PowerShell והשימוש שלהן:

- מחרוזות ב-PowerShell: https://he.wikibooks.org/wiki/PowerShell/מחרוזות
- חיבור מחרוזות ב-PowerShell: https://www.red-gate.com/simple-talk/sysadmin/powershell/string-formatting-in-powershell/
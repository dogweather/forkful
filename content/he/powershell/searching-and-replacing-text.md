---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?

חיפוש והחלפת טקסט הוא לקחת מחרוזת טקסט מסוים ולהחליף אותה במחרוזת אחרת. זה מאד שימושי למתכנתים, שכן הם לעיתים קרובות צריכים לשנות את הקוד על ידי שינויים קטנים ברחבי הקוד.

## כיצד לעשות:

ב-PowerShell, אנו משתמשים בפקודה `-replace`, כאשר המשתנה הראשון הוא הטקסט שאנחנו רוצים למצוא, והמשתנה השני הוא מה שאנו רוצים להחליף בו. 
תראו את הדוגמה להלן:

```PowerShell
$data = 'שלום, אני גר בתל אביב.'
$newdata = $data -replace 'תל אביב', 'הרצליה'
$newdata
```

תוצאת הקוד הזה תהיה:
```
שלום, אני גר בהרצליה.
```

## צלילה עמוקה

הוספת האפשרות לחליף טקסט בשורת פקודה הייתה שדרוג חשוב בעולם הפקודות. כלול עם שחרור PowerShell 2.0, זה החליף את החשיבות הבלתי נמנעת של פיתוח אסטרטגיה מורכבת של סקריפטים כדי להציג באופן מלאכותי את האפשרות הזו.

ישנם חלופות לפקודה `-replace`, כמו `String.Replace()` ו `Regex.Replace()`, אך `-replace` נותן לך יותר גמישות עם שימוש קל יותר.

סידור התווים הספיציפי בפרמטרים חיוני לשימוש נכון בפקודה `-replace`. אם תחליף ביניהם, לא תקבל את התוצאה שאתה מחפש.

## ראה גם

2. [Replace Operator](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_operators?view=powershell-7.1#replace-operator)
3. [Regex.Replace Method](https://docs.microsoft.com/dotnet/api/system.text.regularexpressions.regex.replace?view=net-5.0)
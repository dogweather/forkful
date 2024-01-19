---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה זה ולמה?

יצירת קובץ זמני היא שיטה שבה מתכנת מייצר קובץ לשימוש שיטתי ובזמן הקצר. זה שימושי כאשר רוצים לאחסן נתונים לאורך זמן כזה שלא אורך זמן הישיבה. 

## הדרכה

```C#
using System.IO;

public string CreateTempFileWithText()
{
    string tempPath = Path.GetTempPath();
    string tempFile = Path.GetTempFileName();

    File.WriteAllText(tempFile, "זהו קובץ זמני.");
    
    return tempFile;
}
```

בהנחה שהפונקציה נקראת, היא תייצר קובץ במיקום הזמני של המערכת ותכתוב את מחרוזת "זהו קובץ זמני." בתוך הקובץ.

## שירותי מידע 

נוסדה במקור במערכת ההפעלה דוס כדי לאפשר לתוכנות לדרוס נתונים לא חוזרים ולא נשנים (כמו לוגים). חלופות כוללות את שימוש בממשקי משתמש גרפיים לבחינה של נתונים לאורך זמן הישיבה, אך יצירת קבצים זמניים משמשת את המטרה יעילות. בעוד אירוניה, מרבית המערכות ההפעלה היום מנהלות קבצים זמניים באופן אוטומטי כך שהמתכנת לא צריך להיות מודע להם.

## ראה גם

Path.GetTempFileName(): https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename
Path.GetTempPath(): https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettemppath
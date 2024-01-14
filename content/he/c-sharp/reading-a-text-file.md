---
title:                "C#: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה

קריאת קובץ טקסט היא כלי חיוני בתכנות בשפת C#. הוא מאפשר לנו לקרוא ולהעביר מידע מקובץ טקסט לתוך קוד ולעבוד איתו באופן יעיל ומהיר. אם אתה מחפש דרך פשוטה ויעילה לקרוא ולעבוד עם קבצי טקסט, הקריאה הזו לך!

## כיצד ל

תחילה, נפתח את הקובץ המבוקש באמצעות הפעולה `File.OpenText()` כדי ליצור אובייקט של `StreamReader`. נשתמש בפעולת `ReadLine()` כדי לקרוא כל שורה בקובץ ולאחר מכן נעבור אל השורה הבאה. כדי להשלים את הקריאה, נשתמש בפעולת `Close()` כדי לסגור את הקובץ. הנה דוגמה של קוד קל לקריאת קובץ טקסט ובחזרה כל התוכן שלו:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        using (StreamReader reader = File.OpenText("myfile.txt"))
        {
            string line;

            while ((line = reader.ReadLine()) != null)
            {
                Console.WriteLine(line);
            }
        }
    }
}
```

### פלט דוגמה:

```
שורה ראשונה של הקובץ
שורה שנייה של הקובץ
שורה שלישית של הקובץ
...
ו כן להלן.
```

## כיוותר

כאשר אנחנו קוראים קובץ טקסט, המחשב מפעיל `StreamReader` כדי לקרוא את התוכן של הקובץ תו אחר תו עד שיגיע לסוף הקובץ. זה בדיוק מה שאנחנו צריכים כתכנתים - גישה נמוכה לקובץ. ניתן להשתמש גם בפעולת `ReadToEnd()` כדי לקרוא את כל התוכן מיד, אך כדי לא לעמוד על כל התוכן בזמן מתאים העדכונים ועל כך נדבר בעתיד.

## ראה גם

- [דוגמאות לקריאת קובץ טקסט עם C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-read-a-text-file-line-by-line)
- [מידע נוסף ע
---
title:    "C#: יצירת קובץ זמני"
keywords: ["C#"]
---

{{< edit_this_page >}}

# למה:

יצירת קובץ זמני היא כלי שימושי כאשר מתכנתים מחפשים דרך לשמור נתונים בזמן ריצת התוכנית המקורית. הקובץ הזמני ישמש כעתק לנתונים, איפה אנו נוכל לכתוב ולקרוא עד שנסיים להשתמש בקבצים נוספים בהמשך.

# איך לעשות:

```C#
// שם קובץ זמני
string tempFileName = "temp.txt";

// יצירת קובץ זמני חדש
File.Create(tempFileName);

// כתיבת נתונים לקובץ זמני
using (StreamWriter writer = new StreamWriter(tempFileName)) 
{
    writer.WriteLine("שלום עולם!");
}

// קריאת נתונים מהקובץ הזמני 
using (StreamReader reader = new StreamReader(tempFileName)) 
{
    string line = reader.ReadLine();
    Console.WriteLine(line);
}

// מחיקת הקובץ הזמני כשהתוכנית נגמרת
File.Delete(tempFileName);
```

פלט צפוי:
```
שלום עולם!
```

# לטפל בזה מתחת:

יצירת קובץ זמני היא עקרונית פשוטה, אך חשוב לזכור כמה נקודות בעת ביצוע התהליך. ראשית, חשוב לוודא שכתיבת הנתונים לקובץ זמני נכונה על ידי עקיפת איות מבני הלשון או שמירת קובץ הנתונים בפורמט נכון. השתמשו ב-`try-catch` כדי להתייחס לאפשרות שהתהליך יכול להיכשל בזמן הריצה. ובכל סופו של דבר, זכרו למחוק את הקובץ הזמני עם סיום התוכנית כדי לא להשאיר נתונים מיותרים במערכת הקבצים שלכם.

# ראו גם:

- [כיצד לעבוד עם קבצים בשפת C#](https://docs.microsoft.com/he-il/dotnet/csharp/programming-guide/file-system/)
- [מדריך לפקודת `File.Create` של C#](https://docs.microsoft.com/he-il/dotnet/api/system.io.file.create?view=netcore-3.1)
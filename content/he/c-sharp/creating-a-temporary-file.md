---
title:    "C#: יצירת קובץ זמני"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

#למה

למידע שימושי ואפשרות לעבודה עם קבצים זמניים בתוכנןה בשפת C#. הקבצים הזמניים משמשים ללא כתוב הן לשמירת מידע זמני במחשב ולאחר מכן מיקוד בצורה יעילה ונוחה. 

##איך להשתמש

```C#
//הגדרת תיקיית מסלול לקובץ זמני
string tempFolderPath = "C:\\Temp";

//יצירת קובץ זמני חדש עם הסיומת .txt
string tempFilePath = Path.Combine(tempFolderPath, "tempFile.txt");
//נכתוב תוכן לקובץ זמני
File.WriteAllText(tempFilePath, "זהו קובץ זמני חדש");

//קריאת תוכן מקובץ זמני
string tempFileContents = File.ReadAllText(tempFilePath);
Console.WriteLine(tempFileContents);

//מחיקת הקובץ הזמני
File.Delete(tempFilePath);
```

תוצאה:

```C#
זהו קובץ זמני חדש
```

## העמקת הנושא

יצירת קבצים זמניים היא תהליך פשוט ושימושי בשפת C#. ישנם כמה דרכים שונות ליצור קבצים זמניים, כולל השימוש בספריית האותות "System.IO" המכילה מספר שיטות ופונקציות לניהול קבצים. למשל, ניתן להשתמש בשיטת `GetTempFileName()` ליצירת שם קובץ זמני אקראי ולאחר מכן להשתמש בפונקציות נוספות כמו `Create()` ליצור את הקובץ בתיקייה המתאימה.

##ראו גם

- [מדריך ליצירת קובץ זמני ב-C#](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename?view=net-5.0)
- [הסבר על קבצים זמניים ב-C#](https://www.c-sharpcorner.com/uploadfile/b942f9/working-with-temporary-file-in-C-Sharp/)
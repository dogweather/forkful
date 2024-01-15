---
title:                "יצירת קובץ זמני"
html_title:           "Swift: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה?

למה מישהו היה רוצה ליצור קובץ זמני בקוד סוויפט? ייתכן שיהיה צורך לבדיקה מהירה של פונקציונליות חדשה או לאחסון נתונים זמניים לפני שמעבירים אותם לקובץ קבוע.

## כיצד לעשות זאת

כדי ליצור קובץ זמני בשפת סוויפט, ניתן להשתמש במחלקת "FileManager" ובפונקציה "url(for:in:appropriateFor:create:)". לדוגמה, ניתן להשתמש בקוד הבא:

```Swift
let temporaryDirectoryURL = FileManager.default.temporaryDirectory
let temporaryFileURL = temporaryDirectoryURL.appendingPathComponent("myTempFile").appendingPathExtension("txt")
```

בקוד זה, אנחנו יוצרים נתיב לתיקייה זמנית באמצעות פונקציית "temporaryDirectory" ונוסיף לו שם וסיומת לקובץ זמני. לאחר מכן, ניתן להשתמש בנתיב זה כדי ליצור את הקובץ הזמני בפונקציית "url(for:in:appropriateFor:create:)" על ידי ספציפית את התיקייה הזמנית ואת השם של הקובץ. ניתן לראות את כתובת הנתיב המלא של הקובץ בתוצאת הפלט של פונקציית "size" כדי לוודא שהוא נוצר בהצלחה:

```Swift
print("Temporary file created at: \(temporaryFileURL.path)")
```

תוצאה: Temporary file created at: /private/var/folders/nz/t7_yjdwn_wb2jl95089c8bbw0000gn/T/myTempFile.txt

## עיון מעמיק

כאשר יוצרים קובץ זמני בשפת סוויפט, הוא נשמר באיזור הזמן הזמני של המכשיר ולא באיזור הזיכרון הקבוע. זה יכול להיות שימושי לאפליקציות שדורשות זיכרון זמני לפעולות כמו עריכת תמונות או טעינת נתונים מאינטרנט. חשוב לזכור שקבצים זמניים נמחקים באופן אוטומטי במערכת ההפעלה, לכן חשוב לשמור על הנתונים החשוב
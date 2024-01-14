---
title:                "Swift: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## למה

כתיבת קובץ טקסט היא מנגנון חשוב בתכנות בשפת Swift. היא מאפשרת לנו ליצור קבצים מבחינה דינמית, לקבל ולשלוח מידע ולעבדו בקלות. בכתיבת פוסט זה נלמד כיצד לכתוב ולקרוא קובץ טקסט בשפת Swift, כדי שתוכלו להשתמש בכך בתוכניות שלכם.

## איך לעשות זאת

תחילה, נצטרך להגדיר משתנה שיכיל את המילת המפתח של הקובץ שברצוננו ליצור. לדוגמה, נגדיר את המשתנה `filePath` כדי לבחור את המיקום והשם של הקובץ. נשתמש בפונקציה `FileManager` כדי ליצור את הקובץ ולהגדיר את התוכן שלו, בעזרת הפונקציה `createFile`. הנה דוגמה לקוד שמציג את הצעדים הנדרשים ליצירת קובץ טקסט בשפת Swift:

```Swift
let fileManager = FileManager.default
let filePath = fileManager.urls(for: .documentDirectory, in: .userDomainMask)[0].appendingPathComponent("myFile.txt")
let text = "כתוב כאן את הטקסט שברצונך לכתוב לקובץ"
do {
    try text.write(to: filePath, atomically: true, encoding: String.Encoding.utf8)
} catch {
    print("לא ניתן ליצור את הקובץ")
}
```

נדון גם באופן שבו ניתן לקרוא קובץ טקסט קיים באמצעות הפונקציה `String(contentsOf: filePath, encoding: .utf8)` ולהדפיס את התוכן שלו. הנה דוגמה לקוד:

```Swift
do {
    let text = try String(contentsOf: filePath, encoding: .utf8)
    print(text)
} catch {
    print("לא ניתן לקרוא את הקובץ")
}
```

כמו כן, אפשר למחוק קובץ טקסט באמצעות הפונקציה `removeItem(at: filePath)`. נגיע עוד עמוק יותר עם ניתוח התכונות של מחלקת `FileManager` וכיצד להשתמש בהן בכתיבת קבצים טקסט בשפת Swift. ניתן גם לעבוד יחד עם
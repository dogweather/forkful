---
title:                "יצירת קובץ זמני"
html_title:           "TypeScript: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

מה ולמה?

יצירת קובץ זמני היא פעולה שמאפשרת למתכנתים ליצור ולשלוח קובץ זמני בזמן ריצת התוכנית. זה נעשה על מנת לשמור על מצב זמן מסוים בכדי למנוע בעיות תחבורה וחוסר אבטחה.

איך לעשות?

עבור הדוגמה הבאה, נשתמש בפונקציה המובנית `fs` בתוך Typescript כדי ליצור קובץ זמני ולהדפיס את נתוניו:

```TypeScript
import * as fs from 'fs';

let tempFile = fs.writeFileSync('temp.txt', 'Temp file created!');

console.log(tempFile);
```

תוצאה:

```
Temp file created!
```

להמשך קריאה על איך לעבוד עם קבצי טקסט זמניים בכדי להיות יצירתיים עם הקוד שלכם, אני ממליץ לך לבדוק את [מדריך JavaScript זמני של Mozilla](https://developer.mozilla.org/he/docs/Learn/JavaScript/Client-side_web_APIs/Client-side_storage) ואת תיעוד המפורט של `fs` עבור פייתון.

מעמקים עמוקים

יצירת קובץ זמני היא קידום עצמי של יצירת קבצים תחת מחשב נתונים על מנת לשמור עותקים עבור המתמשים המאושרים בכוח המחשב. הישנון של שיטת יצירת קובץ נפרדים זמנה הוא נושא בו התפתחה ההיסטוריה של קבצי תחבורה במטרה להאצן את יכולת קבוצות העבודה לעבוד עם המאפיינים שנדרשים בקובץ זמני. ישנם דברים רבים שאנחנו יכולים ללמוד על יצירת קבצים זמניים, במקום לנסות לכתוב את הכל בצורה הזו, אי אפשר לבטל את התובנה הבסיסית של שימוש בקבצי טקסט זמניים כשאנו עובדים עם נתונים בתוך פורמט ספציפי וסטניאלי.

ראה גם

- [תוכניות ממוחשב של Microsoft](https://docs.microsoft.com/en-us/windows/text/temporary-files)
- [תיעוד Python של FileSystem](https://docs.python.org/3/library/os.html)
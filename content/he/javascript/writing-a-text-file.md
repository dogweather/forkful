---
title:                "Javascript: הכנת קובץ טקסט"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## למה

כתיבת קבצי טקסט היא חלק חשוב ובסיסי מאוד בתכנות ב־JavaScript. זהו הדבר הראשון שמי שמתחיל ללמוד תכנות ילמד, וזה חייב להיות חלק מכל תוכנית או אפליקציה שכותבים. כתיבת קבצי טקסט מאפשרת לנו לאחסן מידע, לטעון אותו לתוכנית ולעבוד איתו.

## איך לעשות זאת 

תחילה נצטרך ליצור אובייקט חדש שיכיל את רכיבי הקובץ. נחזיק את רכיבי הקובץ בשיטה המקובלת ביותר - יש ליצור אובייקט חדש בשם "file" ולשים בתוכו את התוכן שאנחנו רוצים לשמור. לדוגמה:

```Javascript
var file = "זהו קובץ טקסט ראשון שלי ב־Javascript";
```

אחר כך, נשתמש בשיטת writeFile כדי לייצר את הקובץ עם התוכן שרצינו לשמור. ניתן לכתוב קבוצות של ביטויי תנאים למטרה זו, אך במקרה שלנו זה יהיה פשוט 2 שורות בלבד:

```Javascript
fs.writeFile('file.txt', file, function (err) {
  if (err) throw err;
  console.log('קובץ הטקסט נוצר בהצלחה!');
});
```

בתור הפרמטרים של writeFile נכנס השם של הקובץ החדש (שערכנו לפני כן ל־"file.txt") ואת התוכן שרצינו לשמור. במקרה הזה, נשתמש בפונקצית callback כדי להדפיס הודעה שהקובץ נוצר בהצלחה. למידע נוסף על פונקציות של כתיבת קבצים ב־Javascript ניתן לקרוא [כאן](https://www.geeksforgeeks.org/javascript-reading-file-using-filereader/).

## העומק

כפי שציינו למעלה, תכנות ב־Javascript הוא כולל כמה שכונות שונות, כולל כתיבת קבצי טקסט. הפעם, תצטרכו לבחור בין כ
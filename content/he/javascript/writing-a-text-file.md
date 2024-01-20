---
title:                "כתיבת קובץ טקסט"
html_title:           "Javascript: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת קובץ טקסט היא תהליך שבו מתכנתים יוצרים קובץ שמכיל טקסט פשוט עם מידע שונה. מתכנתים משתמשים בכתיבת קבצי טקסט על מנת ליצור מידע שייכן בקבצים לשימוש עתידי.

## איך לעשות:
כדי לכתוב קובץ טקסט ב-Javascript, ניתן להשתמש בפקודת writeFile ממודול fs. למשל, ניתן ליצור קובץ טקסט עם המידע "Hello World!" באמצעות הפקודה הבאה:

```Javascript
fs.writeFile("myFile.txt", "Hello World!", function(err) {
  if (err) throw err;
  console.log("קובץ טקסט נוצר בהצלחה!");
});
```

כאשר נפעיל את הקוד, קובץ נקרא "myFile.txt" ייוצר ויכיל את המסר "Hello World!".

## שקוף לתהות:
כתיבת קבצי טקסט אינה חדשה ונעשתה כבר מזמן רב לפני התחדשות הטכנולוגיות שיש לנו כיום. אם לא תרצו להשתמש ב-fs כדי לכתוב קבצי טקסט, ניתן להשתמש במודולים אחרים כמו stream ו-buffers. גם קבצי טקסט ניתן לכתוב באמצעות לולאות המדונים (for loops) או פונקציות כמו write (וגם בהם ניתן להשתמש על ידי למידה עיוורת מקודמת למעט שאלות מתן).

## ראו גם:
כדי ליצור קבצי טקסט ב-Javascript ניתן להשתמש בכל מודול כמו fs ואפילו בשיטות של JQuery כגון post או ajax. תחשבו על כמה דברים יעילים כאשר מותאמים לשימוש עתידי (כגון writeStream) שאמות מכאן עקבים לכמה קירות שניצלו? במקרה הזה אנשים קריאים לשימושים של המיצוי קטעים במתיחות שמוחים רווחים (והמשתעממים מהאמות מספיק כדי להדהד עם fs משופע פעמים רבות). איך אפשר לשפר את יכולת המעטפה בכדי לעשות דברים כמו（4.0或說，或7.2）？

בנוסף, ניתן ללמוד עוד על כתיבת קבצי טקסט בקישורים הבאים:

- [פקודת fs.writeFile המפורטת במסמך הרשמי של ג'אווהסקריפט](https://nodejs.org/dist/latest-v6.x/docs/api/fs.html#fs_fs_writefile_file_data_options_callback)
- [הפרדה בין טקסט לקבצי טקסט עם JQuery](https://stackoverflow.com/questions/16657935/how-to-pass-from-html-to-jquery-ajax-php-the-method-post-without-an-express)- [שימוש בתוכנית משתמש של fs להשתמש בקבצי טקסט](https://github.com/lodash/lodash/blob/4.17.11-npm/misc/scripts/build-dist.bundle.js)
- [Node.js תיאור קודי קומפיות WriteStream באבטחה על הכתובת 난 팔다의 쥐죽
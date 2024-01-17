---
title:                "הדפסת פלט דיבאג"
html_title:           "C++: הדפסת פלט דיבאג"
simple_title:         "הדפסת פלט דיבאג"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?
הדפסת פלט דיבאג היא פעולה שמתבצעת על ידי מפתחי תוכנה כדי לעזור להמציא באגים וללא כלות שימוש. מפתחים מתנסים בהפעלת קוד בדיקה שמדפיסה ערכים ומידע נוסף על תוכניות מסוימות כדי לבדוק אם הם פועלים כמצופה.

## איך לבצע:
כאשר משתמשים בשפת C++, ישנם מספר דרכים להדפסת פלט דיבאג. הנה כמה דוגמאות:

```C++
// הדפסת מספר שלם לשם בדיקת פעולת תנאי
int num = 5;
cout << "המספר הוא " << num << endl;

// הדפסת ערך בוליאני לשם בדיקת שגיאה
bool flag = true;
cout << "הערך הוא " << flag << endl;

// הדפסת ערך מצבי להדגשת נקודות דפוס
int pos[] = { 1, 2, 3 };
for (int i = 0; i < 3; i++) {
    cout << "המיקום הנוכחי הוא " << pos[i] << endl;
}
```

אם אתה רוצה להדפיס מוצעים מסובכים יותר, אתה יכול להשתמש בפונקציות ייעודיות כמו `printf` או `sprintf`.

## לעמוד בעומק:
במודע כי בעבר, מפתחים היו נאלצים להימנע מהדפסת פלט דיבאג בשביל לחסוך בזמן ומשאבים. במאמר זה, אנו צורכים בדרך נקייה ויעילה יותר באמצעות פונקציות ייעודיות. אולם, דבר נכון לגירסת C++ 11 אין צורך לדאוג מכך, כי השפה מציעה דפסות דירקטיות פשוטות יותר. בנוסף, ישנם כלים נוספים כגון מניפולציות בדפוס עם הפונקציות `cerr` ו`clog` כדי להדגיש חשיבה לכוחות הביצוע.

## ראה גם:
- [איך לבצע Debug ב-C++](https://www.geeksforgeeks.org/how-to-debug-c-programs-using-gdb-in-linux/)
- [שיטות להוצאת מידע מזמין ב-C++](https://www.geeksforgeeks.org/methods-to-output-the-contents-of-a-vector-in-c/)
- [מדריך לחדר Debugging ב-Visual Studio](https://docs.microsoft.com/en-us/visualstudio/debugger/debugging-techniques-in-visual-studio?view=vs-2019)
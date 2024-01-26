---
title:                "שימוש במנפה שגיאות"
date:                  2024-01-26T03:48:17.823580-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במנפה שגיאות"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/using-a-debugger.md"
---

{{< edit_this_page >}}

## מה ולמה?
מנפה תקלות (דיבאגר) הוא כלי שמאפשר לך לבדוק את הקוד שלך בשפת C בעת הריצה, צעד אחר צעד, כדי לחפש תקלות. מתכנתים משתמשים במנפי תקלות כדי להבין כיצד הקוד שלהם מתנהג, לתקן בעיות ולשפר ביצועים מבלי לשחק במשחק ניחושים.

## איך לעשות זאת:
נניח שאתה עובד על תוכנית C פשוטה שמחשבת את העצרת של מספר, אבל יש בעיה. כדי להשתמש במנפה תקלות כמו `gdb` (GNU Debugger), קודם כל קמפל עם הדגל `-g` כדי לכלול מידע לדיבאג:

```c
// לקמפל עם: gcc factorial.c -o factorial -g
#include <stdio.h>

long factorial(int n) {
    if (n < 0) return 0; // בדיקה פשוטה לקלט שלילי
    long result = 1;
    while (n > 1)
        result *= n--;
    return result;
}

int main() {
    int number = 5;
    long result = factorial(number);
    printf("העצרת של %d היא %ld\n", number, result);
    return 0;
}
```

לאחר מכן, הרץ אותה ב-gdb:

```shell
$ gdb ./factorial
```

הגדר נקודת עצירה בפונקציה `factorial` והרץ את התוכנית:

```gdb
(gdb) break factorial
(gdb) run
```

כאשר היא מגיעה לנקודת העצירה, עבור דרך כל שורה באמצעות `next` או `n` ובדוק משתנים עם `print` או `p`:

```gdb
(gdb) next
(gdb) print result
$1 = 1
```

תוצאה דוגמית תספק ערכים בזמן אמת וזרימת ביצוע התוכנית.

## צלילה עמוקה
מנפי תקלות היו קיימים משנות ה-60, והתפתחו ממוניטורים פשוטים ליישומים מורכבים מבוססי GUI. דיבאג באמצעות הדפסה היה נפוץ לפני שפותחו מנפי תקלות בשלים. חלופות ל-`gdb` כוללות את `lldb`, `dbx`, או מנפי תקלות משולבי סביבת פיתוח, כמו אלו ב-Visual Studio או CLion.

כאשר מתמודדים עם מנפי תקלות, המימוש משתנה—חלקם יכולים לתפוס שגיאות בזמן ריצה, לבחון זיכרון, או אפילו להפוך את כיוון הביצוע של תוכנית. `gdb` יכול להתחבר לתהליכים רצים, מה שמאפשר דיבאג של תוכנה הנמצאת כבר בריצה, יתרון לתיקון תקלות במערכות חיות.

## ראה גם
- GNU Debugger (GDB): https://www.gnu.org/software/gdb/documentation/
- Debugging with GDB: https://sourceware.org/gdb/current/onlinedocs/gdb
- LLDB Debugger: https://lldb.llvm.org/use/tutorial.html
- טכניקות דיבאג ב-C: http://www.cprogramming.com/debugging/debugging.html
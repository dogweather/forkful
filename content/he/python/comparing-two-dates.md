---
title:    "Python: השוואת שתי תאריכים."
keywords: ["Python"]
---

{{< edit_this_page >}}

### למה:
מתוך סקרנות ודרישה למידע נוסף, אנשים יעשו בדיקה והשוואה בין שתי תאריכים כדי להבין את ההבדלים ביניהם. יתכן גם שאנשים ירצו לבדוק אם תאריך מסוים הינו יום שישי או שבת, או כאשר הם צריכים לטפל בנתון תאריך וליצור חישובים לכוונת משימות יומיות או כדי לנתח נתונים סטטיסטיים.

### איך לעשות:
```python
# ייבוא ספריית התאריך datetime
import datetime

# פונקציה ליצירת אובייקט תאריך
date1 = datetime.datetime(2020, 6, 22)

# פונקציה ליצירת אובייקט תאריך אחר
date2 = datetime.datetime(2019, 10, 3)

# הדפסת התאריכים להשוואה
print("תאריך ראשון: ", date1)
print("תאריך שני: ", date2)

# השוואת בין שני התאריכים והדפסת התוצאה
if date1 > date2:
    print("תאריך ראשון איחוד גדול מן התאריך השני")
else:
    print("תאריך שני איחוד גדול מן התאריך הראשון")
```

פלט:
```
תאריך ראשון:  2020-06-22 00:00:00
תאריך שני:  2019-10-03 00:00:00
תאריך ראשון איחוד גדול מן התאריך השני
```

### פיתוח מעמיק:
בעקבות שיפורים בטכנולוגיות שונות, אנו כעת יכולים להשוות תאריכים בין יותר משתי יחידות זמן. לדוגמה, אנו יכולים להשוות בין תאריך ושעה, בין תאריך ובין תאריך ויום בשבוע. השימוש בפונקציות שמגיעות עם ספריית datetime עשוי לא להיות מספיק לפתרון כל סוגי ההשוואות. בשל כך, ייתכן שיהיה צורך להשתמש בפונקציות נוספות כגון פונקציות להמרת
---
title:                "חישוב תאריך בעתיד או בעבר"
date:                  2024-02-01T21:50:13.496621-07:00
model:                 gpt-4-0125-preview
simple_title:         "חישוב תאריך בעתיד או בעבר"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/vba/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר כרוך בקביעת תאריך הנמצא במרחק מספר מסוים של ימים, חודשים או שנים מתאריך נתון. מתכנתים לעיתים קרובות זקוקים לפונקציונליות זו כדי לאוטמט אזכורים, מנויים, תאריכי תפוגה, ולתזמן משימות ביישומים שונים.

## איך לעשות זאת:
ב-Visual Basic for Applications (VBA), הפונקציה העיקרית המשמשת לחישוב תאריכים בעתיד או בעבר היא `DateAdd()`. פונקציה זו מוסיפה מרווח זמן מסוים לתאריך, ומחזירה תאריך חדש.

הנה דוגמא בסיסית להוספת 10 ימים לתאריך הנוכחי:

```vb
Dim futureDate As Date
futureDate = DateAdd("d", 10, Date) ' מוסיף 10 ימים לתאריך הנוכחי
Debug.Print futureDate ' מוציא: 20/04/2023 לדוגמה
```

באופן דומה, למציאת תאריך 10 ימים בעבר:

```vb
Dim pastDate As Date
pastDate = DateAdd("d", -10, Date) ' מחסיר 10 ימים מהתאריך הנוכחי
Debug.Print pastDate ' מוציא: 31/03/2023, בהנחה שהיום הוא 10/04/2023
```

הדוגמאות הללו פשוטות למדי. ניתן להחליף את `"d"` בקודי מרווחים אחרים, כגון `"m"` עבור חודשים ו-`"yyyy"` עבור שנים, כדי לבצע חישובי תאריכים שונים. הנה דוגמה לחישוב של תאריך שנה אחת בעתיד:

```vb
Dim nextYear As Date
nextYear = DateAdd("yyyy", 1, Date) ' מוסיף שנה אחת לתאריך הנוכחי
Debug.Print nextYear ' מוציא: 10/04/2024 אם היום הוא 10/04/2023
```

## עיון מעמיק
הפונקציה `DateAdd` היא חלק בלתי נפרד מ-VBA מאז היווסדה, מקורה בשפת התכנות BASIC הקודמת לה. למרות שהיא מציעה פשטות להוספה או חיסור של מרווחי זמן מתאריכים, חשוב לציין כי VBA, כולל פונקציות טיפול בתאריכים שלה, לא תמיד עולות בקנה אחד עם הנוחות או היעילות הנמצאות בשפות תכנות חדישות יותר.

לדוגמה, שפות מודרניות כמו Python עם המודול `datetime` או JavaScript עם ספריות כמו `moment.js` ו-`date-fns` מציעות דרכים אינטואיטיביות וחזקות יותר לניפוי תאריכים. האפשרויות הללו נותנות תמיכה טובה יותר בלוקליזציה, אזורי זמן ושנים מעוברות, דבר העשוי להשתלם יותר ליישומים הדורשים חישובי תאריכים מדויקים בקנה מידה עולמי.

עם זאת, עבור מקרוסי Excel ויישומים הדורשים אינטגרציה תוך כדי המערכת של Microsoft Office, VBA נותר בחירה מעשית. הפשטות בגישה ישירה ובניפוי נתוני Excel היא יתרון משמעותי. בנוסף, עבור רוב חישובי התאריכים הבסיסיים כגון תזמון ותזכורות, `DateAdd()` ב-VBA מספקת פתרון ראוי ופשוט. התחביר שלה קל להבנה למתחילים, בעוד האינטגרציה שלה ביישומי הסוויטה הרחבה של Office מבטיחה את הרלוונטיות שלה במקרים ספציפיים.

לסיכום, בעוד ששפות תכנות חלופיות עשויות להציע גישות יותר מודרניות לחישוב תאריך, `DateAdd()` ב-VBA מהווה עדות לעמידות השפה בתחומים שבהם היא מאוד נחוצה.
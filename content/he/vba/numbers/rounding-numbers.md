---
title:                "סיבוב מספרים"
aliases:
- /he/vba/rounding-numbers/
date:                  2024-02-01T22:04:47.123952-07:00
model:                 gpt-4-0125-preview
simple_title:         "סיבוב מספרים"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/vba/rounding-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

עיגול מספרים בתכנות מדובר בהקרבה של מספר למספר שלם הקרוב אליו או למספר מסוים של ספרות אחרי הנקודה. מתכנתים מעגלים מספרים כדי לפשט נתונים, לשפר את קריאותם, או לעמוד בקריטריונים נומריים מסוימים בחישובים, במיוחד בחישובים פיננסיים שבהם הדיוק חשוב.

## איך לעשות:

ב-Visual Basic for Applications (VBA), ניתן להשיג עיגול באמצעות מספר פונקציות, כל אחת מתאימה לתרחישים ספציפיים. הנה הפונקציות הנפוצות ביותר עם דוגמאות:

1. **פונקציית Round**:
   פונקציית ה`Round` מעגלת מספר למספר מסוים של ספרות.
   ```basic
   Dim roundedNumber As Double
   roundedNumber = Round(3.14159, 2)  ' פלט: 3.14
   MsgBox roundedNumber
   ```
   
2. **הפונקציות Int ו-Fix**:
   גם הפונקציות `Int` ו-`Fix` נמצאות בשימוש כדי לעגל מספרים למספר השלם הקרוב ביותר למטה, אך הן מתנהגות באופן שונה עם מספרים שליליים.
   ```basic
   Dim intRounded As Integer
   Dim fixRounded As Integer
   
   intRounded = Int(-3.14159)  ' פלט: -4
   fixRounded = Fix(-3.14159)  ' פלט: -3
   
   MsgBox "Int: " & intRounded & ", Fix: " & fixRounded
   ```

3. **פונקציות Ceiling ו-Floor**:
   ב-VBA אין פונקציות מובנות `Ceiling` ו-`Floor` כמו בשפות אחרות. כדי לחקות זאת, יש להשתמש ב-`Application.WorksheetFunction.Ceiling_Math` וב-`Application.WorksheetFunction.Floor_Math` עבור Excel VBA.
   ```basic
   Dim ceilingNumber As Double
   Dim floorNumber As Double
   
   ceilingNumber = Application.WorksheetFunction.Ceiling_Math(3.14159)  ' פלט: 4
   floorNumber = Application.WorksheetFunction.Floor_Math(3.14159)  ' פלט: 3
   
   MsgBox "Ceiling: " & ceilingNumber & ", Floor: " & floorNumber
   ```

## טבילה עמוקה

פונקציית ה`Round` ב-VBA שונה באופן מהותי משיטות עיגול בשפות אחרות בשל שימושה ב**עיגול הבנקאי**. עיגול הבנקאי מעגל למספר הזוגי הקרוב ביותר כאשר המספר נמצא בדיוק באמצע בין שני מספרים, דבר המפחית הטיה בחישובים על מערך נתונים גדול ומספק תוצאה סטטיסטית משמעותית יותר. עם זאת, זה עשוי להוביל להתנהגות בלתי צפויה למי שלא מכיר אותה, במיוחד כאשר דיוק שלם צפוי בכל מקרה.

בניגוד לכך, שפות תכנות רבות ומערכות משתמשות ב"עיגול אריתמטי" או "עיגול למעלה", שבו מספר הנמצא בדיוק באמצע בין שני ערכים אפשריים תמיד יעוגל למעלה. כאשר מתרגמים או מעבירים קוד משפות אחרות ל-VBA, מתכנתים חייבים לשים לב להבדלים אלה כדי למנוע תקלות עדינות או חוסרי דיוק ביישומים פיננסיים וסטטיסטיים.

למרות ש-VBA מציע מגוון פונקציות לעיגול, היעדר הפונקציות `Ceiling` ו-`Floor` (מבלי לפנות ל-WorksheetFunction של Excel) מדגיש חסרון ביכולותיו הטבעיות. מתכנתים הבאים משפות עשירות בתכונות יותר עשויים למצוא את החסרונות האלה כלא נוחים וייתכן שיצטרכו ליישם פתרונות מותאמים או להתאים את חישוביהם לשימוש בפונקציות הזמינות. למרות המגבלות האלה, הבנה ושימוש נכון בפונקציות העיגול של VBA יכולים לעזור להבטיח שחישובים נומריים הם גם מדויקים וגם עומדים בדרישות של רוב היישומים.

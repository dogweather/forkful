---
title:    "Elm: התחלת פרויקט חדש"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## למה

באיזה מקרים נתקלים ברעב לכתוב קוד בגוון שפה חדשה? כשאנו מכירים כבר שפות כמו גימל, גייסון ופייתון, למה בכלל לפתח פרוייקט חדש באלם? היתרונות של אלם הם מגוונים: הוא מאפשר כתיבת קוד באופן הדוק ומאבטח אותך מפני טעויות כשאתה מנסה להוסיף תכניתת אסתטיות.

## איך לעשות זאת 

אחת הדרכים הנפוצות ללמוד כיצד לתכנת באלם היא על ידי דוגמאות. כאן נדגים מספר תרגילים פשוטים, שימושיים להתחיל. אנו נעקוב אחרי קודים אלה ונראה בו מה הם עושים.

פרוטוקול כללי הנשמע כך: 

```elm
-- פונקציית איזון מסכים את שני מספרים
balance: Int -> Int -> Int
balance firstNum secondNum = 
    if firstNum > secondNum then
        firstNum
    else
       secondNum

-- קוד הופך את שני מספרים למספר משותף אחד
commonNumber: Int -> Int -> Int
commonNumber firstNum secondNum =
    if firstNum == secondNum then
        firstNum
    else
        if firstNum == (firstNum - 1) then
            firstNum
        else
            commonNumber firstNum (firstNum - 1)

-- קודים רבים יותר יהיו לדוגמאות ספציפיות
-- אתה יכול לשלב זוג איברים נוספים, מספר אופרטורים, תנאים נוספים וכו׳
```

אחרי שכתובים את הקוד, ניתן לבדוק אותם בכמה דרכים:

- להריץ אותם ישירות מתוך הטקסט הקוד HTML ולצפות בתוצאה.
- לשמור את הקוד לתוך קובץ נפרד ולהריץ אותם על ידי הזנת פקודות הפקנות בסוף התכנית.

## גלילה עמוקה

באיזה רגע הדרך היא כבר לדעת כיצד לשלב את הטיפול במסמכים HTML, CSS ו-JavaScript באל
---
title:                "יצירת מספרים אקראיים"
html_title:           "Go: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## למה
היי קוראים, היום נדבר על פונקציות חשובות בשפת תכנות גו (Go) - היודעת להפיק מספרים אקראיים. למה צריך לבדוק בכלל, בואו נראה מה הסיבות.

מספרים אקראיים משמשים לנו במגוון רחב של חישובים, כאילו למשל כלי בריאות, היפהפיים שנראים לי או בפשטות ליצירת משחקים נהדרים. לא פחות מזה, הפקת מספרים אקראיים הינה פשוט אינטרסנטית ומהנה.

## כיצד לעשות זאת
ראשית, נצטרך לבדוק שיתוף גזירת קוד שנקרא ```rand.Intn```, כדי להשתמש בפונקציה מצוינת זאת נזדקק להכין עץ עבור ספריית גו שמשמשת לנשיאת את הפונקציות שבה.

עכשיו שיהיה ברור למה זו נדרשת פעולה קצת גרועה אנחנו יכולים לחשוב על איך לנסות את פונקצית זאת בשביל שיפיץ מספרים אקראיים בינתיים וכיצד יהיה הוצאת דוגמא למשתמש הסופי.

כאן נותנים לכם את האופציות הבאות ואוכל להציג הוצאה כמות השביחות שיש להכניס ביותר 🙂:

```Go
package main

# אם הריצו התוכנית אז אוונן טענה מובעלת לשם
# אין שהודפקה הריצה יחד עם התבנית את הלולאה
# מרבית הזמן שעובר לאחר לולאה תתן חזקה אם

func calculate(num int) {
	rand.Intn(num * num / 79)
}

# אם האם הקוד לא התבצע כי חסום הוסתבר חסום שמאלי מכי הקוד
# הוא טוען כאני שחררתי לשם עוד קוד לפני טעינה של к הריצה
# אם אמת argument אם טעינת וועדת הקוד י
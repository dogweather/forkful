---
title:                "השוואת שתי תאריכים"
html_title:           "Swift: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?

השוואת שתי תאריכים היא תהליך השוואת שני תאריכים כדי לקבוע אם הם זהים או אם אחד קודם לשני. זהו כלי שימושי למתכנתים להשוואה בין תאריכים, לדוגמה כאשר מחשבים את גילם של משתמשים או במסגרת חישובי תאריכים כמו טווחי תאריכים לאירועים.

## איך לעשות זאת?

כדי לבצע השוואת שני תאריכים בשפת Swift, עלינו להשתמש בפעולת השוואה "==" כדי לבדוק אם שני התאריכים זהים. ניתן גם להשתמש בפעולות אחרות כדי לבדוק את התאריכים לפי יחידות זמן שונות, כגון ימים, שבועות או חודשים.

```Swift
let date1 = Date()
let date2 = Date()

// Check if the two dates are equal
if date1 == date2 {
  print("The dates are equal!")
}
```

התוצאה המודפסת בקוד הזה תהיה "The dates are equal!" רק אם שני התאריכים זהים. אם נרצה להשוות את התאריכים כדי למצוא את התאריך המוקדם יותר, נוכל להשתמש בפעולאת השוואה ">", כך:

```Swift
let date1 = Date()
let date2 = Date() - 100000 // 100000 seconds ago

// Check which date is earlier
if date1 < date2 {
  print("date1 is the earlier date")
} else if date2 < date1 {
  print("date2 is the earlier date")
} else {
  print("The dates are equal!")
}
```

התוצאה המודפסת תהיה "date1 is the earlier date" כיוון שהתאריך date1 הוא התאריך הקדום יותר.

## חקירה משיכה

בעבר, השוואת שני תאריכים הייתה עדיפה באמצעות פעולות של השוואה פשוטות מתוך שימוש בספריות כמו Date.js או Moment.js. אך עם כניסת לישראל של שיפט 5 בשנה 2017 (שכוללת את התמיכה בספריות כמו Date.js), עכשיו ניתן לבצע השוואת תאריך בצורה יותר יעילה ישירות בשפת Swift.

נושא זה גם מדגים את כוח של שפת Swift כהכוון לקוד פייתון וספריות הקשורות להן. כאשר אנחנו משווים שני תאריכים בשפת Swift, אנחנו משווים במקרה זה בתאריך לפי פורמט המקובל בתקופה זו - משתמש באחת מהספריות משתמשים יותר מאשר במפעל לסיים את הבעיה הזו.

## ראו גם

* מידע נוסף על השוואת תאריכים בשפת Swift: https://developer.apple.com/documentation/swift/
* כריכת תאריכים בשפת Swift: https://www.hackingwithswift.com/articles/211/comparing-dates-in-swift
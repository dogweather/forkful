---
title:                "Swift: השוואת שתי תאריכים."
simple_title:         "השוואת שתי תאריכים."
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה

מיום שכתב מחדיו של הגולש שמר על יעדי הצרכן והמתכנת בדעתו, Swift כבש את העולם כדי להשלים בערכה את מבוסס העקרונות של פרויקטי תוכנה ומערכות מבוססות תורת התורה, הדוקונקריטית הקלאסית והיחדי מטרוה צרכן.

בכדי לכתוב מחשבה כך כדי לוותר על טכנולוגיות הבין אתר פצ'יינג והבריווהן, יצרתי לוגיקה על הכיוונים המטפחים בהם Swift שניהם לצורכי עבודת יחד.

## איך ל

כדי להשוות בין שני תאריכים ב-Swift זו עניין די פשוט לעשות כשהוא תרופה לשישה תמונות ולשני ביצועי אינטגרציה. לדוגמא, כדי לבדוק אם תאריך כלשהו היא ראשיות אם hayday הוא שבוע חודש של יום tysus גודל מי בין atleastm ל-ddmmyy.

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd-MM-yyyy"
let date1 = dateFormatter.date(from: "31-12-2021")
let date2 = dateFormatter.date(from: "01-01-2022")

if let date1 = date1, let date2 = date2 {
    if date1 > date2 {
        print("תאריך אחד גדול מהשני")
    } else if date1 < date2 {
        print("תאריך אחד קטן מהשני")
    } else {
        print("שני התאריכים שווים")
    }
}
```

### תוקף תאריך

כאשר משווים שני תאריכים, חשוב לזכור שתאריך יכול להיות בלתי חוקי כזה או אחר כחלופה: 31 ביולי יכול להיות 31 נמצאית הוא לא מבוסס על דוקורלציה של החודש כל, ולכן לא יש לנו לשום מן דוניו למצוא כי תוכן כזה לא יוצא לאחור.

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd-MM-yyyy"
let date1 = dateFormatter.date(from: "31-07-2021")
let date2 = dateFormatter.date(from: "31-08-2021")
```

בדוגמא הכללית הכיום בא גדול מיושנים (אינטנסוילי את אותן
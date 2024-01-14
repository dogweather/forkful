---
title:                "Swift: השוואת שתי תאריכים"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

איחוי: למה לזהות תאריכים מסוימים?

בעולם התכנות, לזהות תאריכים שונים יכול להיות חשוב לטובת פיתוח אפליקציות חכמות ואפקטיביות. אם אתם מחפשים דרך להשוות בין שני תאריכים ב-Swift, זה המדריך המתאים לכם!

כיצד: דוגמאות תכנות ופלט נגזר " ```Swift ... ```"

כדי להשוות בין שני תאריכים ב-Swift, ישנם מספר פעולות ומחלקות שיכולות לעזור לנו בכך. ניתן להשתמש בפעולה "compare" להשוואת תאריכים ולקבל את התוצאה בתור אחת משלושת הראשונות לקיומן: תאריך ראשון קטן מתאריך שני, שני זהה לאחד ושני גדול מראשון. לדוגמה:

```
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let date1 = dateFormatter.date(from: "01/01/2020")
let date2 = dateFormatter.date(from: "01/01/2021")

if let date1 = date1, let date2 = date2 {
    switch date1.compare(date2) {
    case .orderedAscending:
        print("\(date1) הוא קטן מתאריך \(date2)")
    case .orderedSame:
        print("\(date1) זהה לתאריך \(date2)")
    case .orderedDescending:
        print("\(date1) הוא גדול מתאריך \(date2)")
    }
}

//תוצאה: 01/01/2020 זהה לתאריך 01/01/2021
```

כמו כן, ישנם גם מחלקות נוספות כמו "Calendar" ו"DateComponents" שמאפשרות לנו לבצע השוואות מדויקות יותר בין תאריכים. ניתן להשתמש בהן לצורך חישוב כמו ימי חופשה שנותרים בשנת העבודה, למשל. הנה דוגמה:

```
let startDate = Date()
let endDate = Calendar.current.date(byAdding: .month, value: 6, to: startDate)
let daysRemaining = Calendar.current.dateComponents([.day], from: startDate, to: endDate!).day

print("נותרו \(daysRemaining!) ימי החופשה הקובעים לשנת העבודה.")
//תוצאה: נותרו 181 ימי החופשה הקובעים לשנת העבודה.
```

יותר עמוק: מידע נוסף על השוואת תאריכים

הש
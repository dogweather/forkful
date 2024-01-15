---
title:                "המרת תאריך למחרוזת"
html_title:           "Swift: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה
תמיד מותירים את החיים לכתיבת תאריכים ולפעמים זה לא באמת כיף. אבל למרבית החברות הפרסומיות הם יחשו אותו כמו שאתה עושה. למפתחים, יצירת תאריך למחרות היא מנוחה. אך לפעמים יהיה לכם כמה שאתם צריכים להציג את תוכניות הנייר של הארץ. במאמר זה, נדון איך להמיר תאריכים ב- Swift למחרות בכתב.

## איך לעשות
הנה כמה דוגמאות קוד כדי להמיר תאריכים למחרות ב- Swift לכתוב ```Swift
// יצירת פורמט מותאם אישית לתאריך 
let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "dd/MM/yyyy"
let dateString = formatter.string(from: date)
print(dateString) // תוצאה: 03/06/2021

// חישוב תאריך בעבר
let calendar = Calendar.current
let newDate = calendar.date(byAdding: .day, value: -7, to: date)
let newDateString = formatter.string(from: newDate!)
print(newDateString) // תוצאה: 27/05/2021

// המרת תאריך וזמן מקומי לתאריך למחרות
let localDate = formatter.date(from: "01/06/2021")
let convertedDateString = formatter.string(from: localDate!)
print(convertedDateString) // תוצאה: 01/06/2021
```

## Deep Dive
כדי להבין בקלות יותר איך להמיר תאריכים למחרות ב- Swift, נצפה בכמה נקודות מפתח. במשתנה התאריך, אנחנו משתמשים בסוג נתונים של ```Date```, שמייצג תאריך ושעה בלתי פחות. כדי להמיר אותו לתאריך במחרות, אנחנו משתמשים בפונקציות של דפניתר ```DateFormatter```, בהתאם לפורמט שנבחר. בנוסף, אנחנו יכולים להשתמש בכלי עבודה לוודא שהתאריך ניתן לשינוי ושהתוצאה היא תאריך נכון.

## ראו גם
- [DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [NSCalendar](https://developer.apple.com/documentation/foundation/nscalendar)
- [Date](https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html)
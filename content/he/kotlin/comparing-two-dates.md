---
title:                "Kotlin: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה
בהתקופה המודרנית שלנו, תוכניות חישוב משתמשות בזמנים ותאריכים באופן קבוע. זה בדיוק מה שהופך את היכולת להשוות בין שני תאריכים לכלי חשוב למתכנתי קוד.

## איך לעשות זאת
מתשעה דרכים שונות להשוות בין שני תאריכים:

```kotlin
// יצירת תאריך בפורמט dd/MM/yyyy
val date1 = SimpleDateFormat("dd/MM/yyyy").parse("14/03/1992")
val date2 = SimpleDateFormat("dd/MM/yyyy").parse("20/05/1995")

//בדיקה האם תאריך 1 הוא לפני תאריך 2
if (date1.before(date2)) {
    println("תאריך 1 הוא לפני תאריך 2")
}

//בדיקה האם תאריך 1 הוא אחרי תאריך 2
if (date1.after(date2)) {
    println("תאריך 1 הוא אחרי תאריך 2")
}

//בדיקה האם שני התאריכים שווים
if (date1.equals(date2)) {
    println("שני התאריכים שווים")
}

//קבלת הפרש בין שני התאריכים במילי שניות
val diff = date1.getTime() - date2.getTime()
println("הפרש בין שני התאריכים הוא $diff מילי שניות")

// יצירת Calender והשמת התאריך לתוך הקלנדר
val cal1 = Calendar.getInstance()
cal1.setTime(date1)
val cal2 = Calendar.getInstance()
cal1.setTime(date2)

//בדיקה האם יום הוא אותו יום
if (cal1.get(Calendar.DAY_OF_MONTH) == cal2.get(Calendar.DAY_OF_MONTH){
    println("יום זהה")
}

// בדיקה האם חודש ושנה זהים
if (cal1.get(Calendar.MONTH) == cal2.get(Calendar.MONTH) && cal1.get(Calendar.YEAR) == cal2.get(Calendar.YEAR)){
    println("חודש זהה ושנה זהה")
}

// יצירת תאריך נוכחי ובדיקה אם הוא לפני תאריך הנתון
val currentDate = Date()
if (currentDate.before(date1)){
    println("תאריך נוכחי הוא לפני תאריך הנתון")
}
```

פלט:

תאריך 1 הוא לפני תאריך 2
הפרש בין שני התאריכים הוא 951782400000 מילי שניות
יום זהה
חודש זהה ושנה זהה
תאריך נוכחי הוא לפני תאריך הנתון

## יצירה עמוקה
כאשר משתמשים בתאריכים ב
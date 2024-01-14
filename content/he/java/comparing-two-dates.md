---
title:    "Java: להשוואת שתי תאריכים"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

# למה

טפטפו על למה אנחנו משווים שתי תאריכים בתכנות ג'אווה. להשוות שני תאריכים יכול להיות כלי מאוד שימושי כאשר אתם מנסים לבצע מטלות שמתייחסות לזמן כמו לדוגמה הגדרת מספר ימים בין שני אירועים או חישוב גיל בהתאם לתאריך לידה.

# איך לבצע

משווים שני תאריכים בג'אווה ניתן לבצע באמצעות השוואת חלקי התאריך שלהם כמו שנה, חודש ויום. ניתן לקבל את חלקי התאריך הזה באמצעות טיפוס ה-Date בג'אווה.

```java
Date date1 = new Date(2020, 5, 20);
Date date2 = new Date(2021, 5, 20);

if(date1.getYear() == date2.getYear() && date1.getMonth() == date2.getMonth() && date1.getDate() == date2.getDate()){
    System.out.println("The two dates are equal");
} else {
    System.out.println("The two dates are not equal");
}
```
הפלט המשוערת של הקוד שלעיל יהיה: "The two dates are not equal" מכיוון שהתאריכים הם לשני שנים שונות.

# חפירה עמוקה

כאשר אנו משווים שני תאריכים, חשוב לדעת שישנם שני טיפוסים שונים בג'אווה שמייצגים תאריך - טיפוס Date וטיפוס Calendar. טיפוס Calendar מכיל פונקציות נוספות לצורך מניפולציות נוספות עם התאריכים כמו חישוב ימים לפני או אחרי תאריך מסוים.

תוסיפויות לג'אווה 8 יצרו גם את הטיפוס LocalDate המאפשר מניפולציות נוספות לתאריכים כמו חישוב ימים בין שני תאריכים או חישוב גיל בהתאם לתאריך לידה. כדי להשתמש בטיפוס LocalDate יש לייבא את החבילה הבאה:

```java
import java.time.LocalDate;
```

דוגמה לשימוש עם הטיפוס LocalDate:

```java
LocalDate date1 = LocalDate.of(2020, 5, 20);
LocalDate date2 = LocalDate.of(2021, 5, 20);

if(date1.equals(date2)) {
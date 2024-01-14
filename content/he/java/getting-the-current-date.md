---
title:                "Java: לקבלת התאריך הנוכחי"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

# למה
בעולם התכנות, מעין צבירת קודים שאפשר לכתובם בכל שפת תכנות, תומך בהשתמש בתאריך נוכחי בתוכנות שלו. בנוסף, השתמשות בתאריך בתוכנות יכולה להיות שימושית במגוון רחב של מימשיות כמו ליצירת קומפיוטרס לא מעורבים, לתזמן לדוגמה קבצים אוטומטית, וכדומה. 

## איך לעשות את זה
בשפת ג'אווה, ישנם מספר דרכים להשיג את התאריך הנוכחי בתוך קוד התכנית שלנו. הנה כמה דוגמאות כיצד לכתוב קוד לקבלת התאריך הנוכחי בג'אווה:

```java
// דרך אחת היא להשתמש במחלקת Date של ג'אווה
Date currentDate = new Date();
System.out.println(currentDate);
```

```java
// דרך אחרת היא להשתמש במחלקה Calendar של ג'אווה
Calendar calendar = Calendar.getInstance();
Date currentDate = calendar.getTime();
System.out.println(currentDate);
```

```java
// אם מעוניינים לקבל רק את התאריך הנוכחי, ניתן להשתמש במחלקה LocalDate של ג'אווה 8 ומעלה
LocalDate currentDate = LocalDate.now();
System.out.println(currentDate);
```

כאשר מריצים את הקוד הנ"ל, התאריך הנוכחי יודפס בתוך השורה הבאה בפורמט של חודש/יום/שנה:

```sh
Mon Apr 19 17:40:24 IST 2021
```

## עומק היינו
על מנת להבין איך ג'אווה מצליחה להשיג את התאריך הנוכחי, עלינו להבין כיצד מערכת ההפעלה שלנו מאחסנת את התאריך הנוכחי. מערכת ההפעלה שלנו מכילה שעון פנטיום שמתכנס כל עוד המחשב פועל. כל פעם שמקור חיצוני קורא לתאריך הנוכחי, הפעלה תספק לו את התאריך משעון המחשב שלה.

# ראה גם
- [מחלקת Date של ג'אווה](https://docs.oracle.com/javase/7/docs/api/java/util/Date.html)
- [מחלקת Calendar של ג
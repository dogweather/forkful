---
title:                "פיענוח תאריך ממחרוזת"
html_title:           "Java: פיענוח תאריך ממחרוזת"
simple_title:         "פיענוח תאריך ממחרוזת"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# מה ולמה?
פריסת תאריך ממחרוזת הוא תהליך שבו מתבצע קריאה למחרוזת המכילה תאריך והמרתה לאובייקט של תאריך בפורמט של השפת תכנות Java. תהליך זה נדרש בעיקר כאשר התאריך מועבר כפרמטר או נשתמש בו לעיבוד נתונים.

# איך לבצע?
תחת כותרת זו נציג לדוגמה כמה קוד Java ואת הפלט המצורף כדי להדגים איך לפרוס תאריך ממחרוזת.

```Java
// קוד לפריסת תאריך ממחרוזת שיש בה פורמט של חודש/יום/שנה
SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy");
String dateStr = "04/20/2020"; // מחרוזת המכילה תאריך בפורמט של חודש/יום/שנה
Date date = sdf.parse(dateStr); // פריסת התאריך מהמחרוזת לאובייקט של תאריך
System.out.println(date); // פלט: Mon Apr 20 00:00:00 IST 2020 

// קוד לפריסת תאריך ממחרוזת שיש בה פורמט של שנה/חודש/יום
SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd");
String dateStr = "2020/04/20"; // מחרוזת המכילה תאריך בפורמט של שנה/חודש/יום
Date date = sdf.parse(dateStr); // פריסת התאריך מהמחרוזת לאובייקט של תאריך
System.out.println(date); // פלט: Mon Apr 20 00:00:00 IST 2020 

// קוד לפריסת תאריך ממחרוזת שיש בה פורמט של חודשים ושנים באותיות שנכתבים
SimpleDateFormat sdf = new SimpleDateFormat("MMM dd, yyyy");
String dateStr = "Apr 20, 2020"; // מחרוזת המכילה תאריך בפורמט של חודשים ושנים באותיות שנכתבים
Date date = sdf.parse(dateStr); // פריסת התאריך מהמחרוזת לאובייקט של תאריך
System.out.println(date); // פלט: Mon Apr 20 00:00:00 IST 2020
```

# כיול עמוק
הפריסה של תאריך ממחרוזת היא תהליך נרחב המשתמש בו תאריכים נכתבים בפורמטים שונים. פעם אחת ההתאם הפירוש היה ל "תאריך כרויו ושמו של התאריך" ובכך היה לא פשוט לכתוב תאריכים לפי רצון כי הפירוש של התאריכים היה קשור לממשק המכשיר הנסוי. עם המחשובים התקדמים בתחילת שנות ה- 80-ונתקלו באתגר דומה לענייני תאריכים, שהם תאריכים שנגרסו בתוך מחרוזת והיה דורש להתאים את התאריך והתחלפו לרצון של התאריך. 

# ראה גם
* [תיעוד Java רשמי על פריסת תאריך ממחרוזת] (https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
* [הוראות לקריאה וכתיבה של חלק מתאריכים ב Java] (https://www.baeldung.com/java-dates)
* [ספריה פתוחה לפריסת תאריך] (https://github.com/JodaOrg/joda-time)
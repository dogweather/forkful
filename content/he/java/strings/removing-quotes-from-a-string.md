---
aliases:
- /he/java/removing-quotes-from-a-string/
date: 2024-01-26 03:41:01.422546-07:00
description: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E4\u05D9\u05E8\u05D5\u05E9\u05D4 \u05DC\
  \u05E0\u05E7\u05D5\u05EA \u05DE\u05DE\u05E0\u05D4 \u05D0\u05EA \u05DB\u05DC \u05E1\
  \u05D9\u05DE\u05E0\u05D9 \u05D4\u05DE\u05E8\u05DB\u05D0\u05D5\u05EA\u2014\u05D7\u05D9\
  \u05D3\u05D5\u05D3\u05D9\u05D5\u05EA (' '), \u05D2\u05E8\u05E9\u05D9\u05D9\u05DD\
  \ (\" \"), \u05D0\u05D5 \u05E9\u05E0\u05D9\u05D4\u05DD\u2014\u05DE\u05EA\u05D5\u05DA\
  \ \u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D4\u05D8\u05E7\u05E1\u05D8\u05D5\
  \u05D0\u05DC\u05D9\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D7\u05D8\
  \u05D0 \u05D0\u05EA\u2026"
lastmod: 2024-02-18 23:08:52.695889
model: gpt-4-0125-preview
summary: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05E4\u05D9\u05E8\u05D5\u05E9\u05D4 \u05DC\u05E0\
  \u05E7\u05D5\u05EA \u05DE\u05DE\u05E0\u05D4 \u05D0\u05EA \u05DB\u05DC \u05E1\u05D9\
  \u05DE\u05E0\u05D9 \u05D4\u05DE\u05E8\u05DB\u05D0\u05D5\u05EA\u2014\u05D7\u05D9\u05D3\
  \u05D5\u05D3\u05D9\u05D5\u05EA (' '), \u05D2\u05E8\u05E9\u05D9\u05D9\u05DD (\" \"\
  ), \u05D0\u05D5 \u05E9\u05E0\u05D9\u05D4\u05DD\u2014\u05DE\u05EA\u05D5\u05DA \u05D4\
  \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D4\u05D8\u05E7\u05E1\u05D8\u05D5\u05D0\u05DC\
  \u05D9\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D7\u05D8\u05D0 \u05D0\
  \u05EA\u2026"
title: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
הסרת מרכאות ממחרוזת פירושה לנקות ממנה את כל סימני המרכאות—חידודיות (' '), גרשיים (" "), או שניהם—מתוך הנתונים הטקסטואליים. מתכנתים עושים זאת כדי לחטא את הקלטים, להכין נתונים לאחסון, או לפשט משימות פרסינג בהן המרכאות אינן נחוצות ויכולות להוות בעיה.

## איך לעשות זאת:
בואו נמשוך את המרכאות המטרידות האלה מהטקסט שלנו. נשתמש בשיטת `replace()` עבור התיקונים המהירים וב-regex עבור האתגרים הקשים יותר.

```java
public class QuoteRemover {
    public static void main(String[] args) {
        String stringWithQuotes = "\"שלום, 'עולם'!\"";
        String withoutQuotes = stringWithQuotes.replace("\"", "").replace("'", "");
        System.out.println(withoutQuotes); // שלום, עולם!

        // עכשיו עם regex לחובבי התבניות
        String stringWithMixedQuotes = "\"Java\" ו-'Programming'";
        String cleanString = stringWithMixedQuotes.replaceAll("[\"']", "");
        System.out.println(cleanString); // Java ו-Programming
    }
}
```

## צלילה עמוקה
בעבר, מרכאות במחרוזות לא היו משהו שהפריע מדי—המערכות היו פשוטות יותר, והנתונים לא היו מסובכים כמו היום. עם הופעתם של פורמטים מורכבים של נתונים (JSON, XML) והצורך בהחלפת נתונים, ניהול מרכאות הפך להיות קריטי. אם נדבר על חלופות, ברור שאפשר לכתוב מפענח, לעבור מתו למתו, ולבנות מחרוזת חדשה (יכול להיות כיף ביום גשום). קיימות גם ספריות צד שלישי שיכולות לטפל בזה באופן יותר מתוחכם, מציעות אופציות לברוח מתווים במקום להסיר אותם, או להתמודד עם סוגים שונים של מרכאות בהתאם לאזור. מבחינת היישום, חשוב לזכור שהסרת מרכאות ללא הקשר יכולה לשנות את המשמעות או את מבנה הנתונים—תמיד כדאי לשקול את ה"למה" לפני ה"איך".

## ראה גם
- לצלילה עמוקה יותר בנושא regex, ראו את המסמכים הרשמיים של Java: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html
- צריכים לברוח ממרכאות במקום להסיר אותן? Stack Overflow בשבילכם: https://stackoverflow.com/questions/383551/escape-string-for-sql-insert
- עיבוד JSON ב-Java? סביר להניח שתפגשו לא מעט במרכאות. הנה נקודת התחלה: https://www.oracle.com/technical-resources/articles/java/json.html

---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
המרת תאריך למחרוזת ב-Java, זו הפעולה של שינוי ערך מסוג תאריך לטקסט. תכנתים עושים את זה כאשר הם צריכים להציג את התאריך בצורה מסוימת או לשמור אותו בקובץ טקסט.

## איך לעשות:
להלן דוגמה של קטע קוד שמראה איך להמיר תאריך למחרוזת ב-Java.

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateToString {
    public static void main(String[] args) {
        Date date = new Date();
        SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy");
        String strDate = formatter.format(date);
        System.out.println(strDate);
    }
}
```

הפלט של הקוד הזה יהיה התאריך שבו נעשה הרץ, לדוגמה: 
```
27-12-2021
```

## צלילה עמוקה
המרת תאריך למחרוזת התחילה להשתמש יותר מאז ש-Java 1.1 חודשה. ה- API הישן לניהול תאריכים, java.util.Date, הוא מאוד פחות אינטואיטיבי מה- API החדש, java.time. ישנן ב-Java שיטות אחרות להמרת תאריך למחרוזת אבל השיטה עם SimpleDateFormat היא הרגילה יותר. יש להתחשב בעובדה שישנך ערכים מסוימים שאם הם לא יתמרגמו באופן נכון לטקסט, המקום שבו הם יתמרגמו עלול לא לשמידם.

## ראו גם
קישורים נוספים לקרוא על נושאים קשורים:
1. היסטוריה של רכיבי התאריך והזמן ב-Java: https://en.wikipedia.org/wiki/Java_version_history#Java_8_.28Codename:_Spider.29
2. דוגמאות לשימוש ב-Java SimpleDateFormat: https://www.javatpoint.com/java-simpledateformat
3. מדריך ל-java.time, ה- API לניהול תאריכים וזמנים של Java 8: https://www.baeldung.com/java-8-date-time-intro
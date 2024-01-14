---
title:    "Java: שימוש בביטויים רגולריים"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## למה

מספר סיבות נמצאות למה מתכנתים משתמשים בביטויים רגילים בתוכניותיהם. בעזרתם ניתן למצוא טקסטים מסוימים בצורה מהירה ויעילה, לוקחים חלק חשוב במיוחד בשפות תכנות כמו גם בדוגמאת מיחשב/ת ואינטרנט לאיתור קווים תואמים.

## איך לבצע

ראשית, יש לייבא את המחלקות המתאימות לקוד שלנו על ידי הוספה של השורה הבאה לשורת המיוחדת למנתJava:

```Java
import java.util.regex.Matcher;
import java.util.regex.Pattern;
```

כעת ניתן ליצור אובייקט מסוג "Pattern" ולשים בתוכו את הביטוי הרגיל שלנו. לדוגמה:

```Java
Pattern pattern = Pattern.compile("ab*c");
```

ניתן לשנות את הביטוי בכל רגע על ידי שימוש בפקודה "matcher":

```Java
Matcher matcher = pattern.matcher("abc");
```

לאחר מכן, ניתן להשוות את הביטוי לטקסט נתון ולהציג את התוצאה עם הפקודה "matches":

```Java
System.out.println(matcher.matches());
```

כלומר, נקבל את התשובה "true" אם הטקסט מתאים לביטוי ו"false" אם לא.

## העמקה

הפקודות הנ"ל הן רק דוגמאות פשוטות של שימוש בביטויים רגילים. אתרים רבים מציעים טכניקות ותרגולים לשימוש יעיל יותר בביטויים רגילים בתכנות. אנו ממליצים לחפש מדריכים והסברים נוספים על אתרי האינטרנט השונים.

## ראו גם

- https://www.w3schools.com/jsref/jsref_obj_regexp.asp
- https://www.regular-expressions.info/tutorial.html
- https://regexone.com/
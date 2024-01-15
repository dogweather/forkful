---
title:                "ניתוח HTML"
html_title:           "C: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/parsing-html.md"
---

{{< edit_this_page >}}

## מדוע

למה יש לנו בכלל שימוש בפענוח HTML? הפענוח של מסמכי HTML הוא חלק בלתי נפרד מתהליך התכנות ונחשב לכישור חיוני לכל מפתח תוכנה. במאמר זה, נדון במדוע כדאי ללמוד פענוח HTML ואיך לעשות זאת בשפת C.

## איך לעשות זאת

לפניכם חלקים קצרים עם דוגמאות קוד ופלט, המציגים כיצד ניתן לפענח HTML בשפת C.

```C
#include <stdio.h>

int main() {
  // הפענוח מתחיל בחיפוש לאן שהתחיל התג <title>
  char* str = "<html><head><title>הכותרת של הדף</title></head></html>";
  // מדפיס את המילה שבין התגים title
  printf("הכותרת של הדף היא: %s\n", get_text_between_tags(str, "title"));
  return 0;
}

// פונקציה המקבלת מחרוזת ותג ומחזירה את הטקסט שבין התגים
char* get_text_between_tags(char* str, char* tag) {
  int len = strlen(tag);
  // מציאת התחלת התג
  char* start = strstr(str, tag);
  // מציאת הסוף של התג
  char* end = strstr(start + len, tag);
  // יצירת מחרוזת חדשה שמכילה את הטקסט שבין התגים
  char* text = calloc(end - start - len + 1, sizeof(char));
  strncpy(text, start + len, end - start - len);
  return text;
}
```

הפלט של הקוד הנ"ל יהיה:

```
הכותרת של הדף היא: הכותרת של הדף
```

## למעמיקים לבדוק

למי שמעוניין במידע נוסף על הפענוח של HTML בשפת C, הנה כמה נושאים נוספים לבדיקה:

- כיצד לטפל בתוונתיות שונות בין דפים שונים
- שימוש בסיבים ובמצבים קצה בפענוח HTML
- היכן ניתן למצוא מידע מפורט על יישומים אחרים של פענוח HTML בשפת C

## ראו גם

אלו הם כמה מאמרים נוספים שיכולים לעזור לך להמשיך בלימוד הפענוח של HTML ב
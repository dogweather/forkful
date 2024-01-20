---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# מה & למה?
המחרוזת argv בשפת התכנות C מתייחסת לארגומנטים שניתנים לתוכנית מהשורה הפקודה. אנחנו משתמשים בהם כדי להעביר נתונים לתוכנית כאשר אנחנו מריצים אותה, דבר שמאפשר לנו להתאים את ההתנהגות של התוכנית לפי הצורך.

# איך לבצע:
הנה קוד פשוט שמדגים את שימוש במשתנים argc ו- argv לקריאת ארגומנטים מהשורה הפקודה:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
   for(int i = 0; i < argc; i++) {
      printf("ארגומן #%d: %s\n", i, argv[i]);
   }
   return 0;
}
```

אם אתה מריץ את התוכנית הזו עם הארגומנטים "שלום" ו"עולם", תקבל הדפסה של:

```
ארגומן #0: programname
ארגומן #1: שלום
ארגומן #2: עולם
```

# צלילה עמוקה:
אמנם אנחנו רואים רק שני ארגומנטים בדוגמת הקוד, אך המערכת הפעלה מעבירה אף הודעה, היכן שהארגומן הראשון הוא שם התוכנית עצמה. זה היה מקובל באופן רחב בראשית הימים של התכנות, ונותר כך למען שמירה על תאימות לאחור. אלטרנטיבות נוספות לקריאת ארגומנטים מהשורה הפקודה כוללות שימוש ב-getopt() או argp_parse().

# ראה גם:
- למידע נוסף על טיפול בארגומנטים מהשורה הפקודה, [לחץ כאן](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html#Program-Arguments).
- לדוגמאות קוד נוספות לשפת C, [בקר באתר](https://www.learn-c.org/).
- ישנן שפות תכנות אחרות שמשתמשות בארגומנטים מהשורה הפקודה. לדוגמה, [Python](https://docs.python.org/3/library/argparse.html) ו-[Java](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html).
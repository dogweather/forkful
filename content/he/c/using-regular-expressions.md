---
title:    "C: שימוש בביטויים רגולריים"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

# למה

בתכנות, פעמים רבות נתקלים במצבים בהם אנחנו צריכים לחפש מילים או ביטויים מסוימים בתוך טקסט מסודר. ייתכן שאנחנו רוצים למצוא כל בתי המאפיינים בקובץ מסוים או לסנן תכנים לפי תבניות מסוימות. במצבים אלו ניתן להשתמש בביטויים רגולריים כדי לפתור את הבעיות הנ"ל. במאמר הזה נלמד איך להשתמש בביטויים רגולריים בתכנות בשפת C.

# איך להשתמש

בדרך כלל, בכדי להשתמש בביטויים רגולריים בשפת C יש צורך לכתוב אותם בתוך משתנה במשך הקוד. לדוגמה:

```C
char *regex = "[0-9]+";
```

בדוגמה הנ"ל, המשתנה "regex" מכיל את הביטוי הרגולרי המשמש לחיפוש מספרים בטקסט. בשביל להשתמש בביטויים רגולריים, נצטרך להשתמש בפונקציות כמו "regcomp" ו-"regexec" המאפשרות לנו לבצע חיפושים בטקסט מתאימים לביטוי הרגולרי.

לדוגמה, אם נשתמש במשתנה "regex" מהדוגמה הנ"ל כדי לחפש מספרים בטקסט מסוים, תהיה ניתן להשתמש בקוד הבא:

```C
#include <stdio.h>
#include <regex.h>

int main() {
    char *text = "This is a text that contains numbers like 123 and 456.";
    char *regex = "[0-9]+";

    regex_t re;

    // מימוש דוגמה של שימוש בregcomp
    if (regcomp(&re, regex, 0) != 0) {
        printf("Failed to compile regex!\n");
        return 1;
    }

    // מימוש דוגמה של שימוש בregexec לאיתור מספרים
    regmatch_t matches[10];
    if (regexec(&re, text, 10, matches, 0) == 0) {
        for (int i = 0; i < 10; i++) {
            if (matches[i].rm_so == -1) {
                break;
            }

            // מציאת התוכן של התוצאה החלקית והדפסתה
            for (int j = matches[i].rm_so; j < matches[i].rm_eo; j++) {
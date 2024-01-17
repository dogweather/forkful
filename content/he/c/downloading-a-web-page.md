---
title:                "הורדת עמוד אינטרנט"
html_title:           "C: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# מה ולמה?
הורדת דף אינטרנט היא תהליך שבו משתמשים משתמשים כדי להוריד תוכן מאתר האינטרנט למחשב שלהם. מתכנתים משתמשים בתהליך זה בכדי לגשת למידע ולתוכן מרחוק בצורה פשוטה ומהירה.

# איך לעשות זאת:
למטה תמצאו דוגמאות קוד ותוצאות להורדת דף אינטרנט בקוד C.

```C
#include <stdio.h>

int main()
{
    char url[] = "https://www.example.com"; // כתובת האתר שברצונכם להוריד
    char cmd[50]; // גודל המחרוזת יכול להיות כלשהו, תלוי בגודל האתר שתרצו להוריד 
    
    sprintf(cmd, "wget %s", url); // אתחול והנחת הכתובת של האתר במשתנה cmd באמצעות פונקציית sprintf
    
    system(cmd); // הפעלת פקודת ההורדה במחברת

    return 0;
}
```

תוצאה:

```
--2020-11-20 12:00:00--  https://www.example.com/
Resolving www.example.com (www.example.com)... 93.184.216.34
Connecting to www.example.com (www.example.com)|93.184.216.34|:443... connected.
HTTP request sent, awaiting response... 200 OK
...
```

# כיול לעומק:
בעמוד זה למדנו כיצד להוריד דף אינטרנט בקוד C. כיום, ניתן למצוא מגוון של כלים וטכנולוגיות להורדת דפי אינטרנט בכתובות שונות כמו HTML, CSS ו-JavaScript.

תוכלו להשתמש ב-URL קיימים ובתכני HTML כדי ללמוד עוד על התהליך או לבדוק כמה כלים אחרים שיכולים לעשות את עבודת ההורדה על דף אינטרנט.

לפני ביצוע הורדת הדף, כדאי לוודא שיש לכם גישה לאתר ולוודא שהאתר אינו מכיל כל מיני חסימות או תנאים כדי למנוע גישה לא מורשית לתוכן.

## ראו גם:
- [כיצד להוריד דף אינטרנט בשפת פייתון](https://realpython.com/python-web-scraping-practical-introduction/)
- [מאמר על טכנולוגיות אינטרנט שונות](https://www.webdesignerdepot.com/2019/02/a-guide-to-different-web-technologies-and-when-to-use-them/)
---
title:                "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מדוע

שליחת בקשת HTTP עם אימות בסיסי (basic authentication) היא דרך נוחה ובטוחה לאמת שתי צדדים שמתקשרים ביניהם. באמצעות אימות הבסיסי, משתמש לא מוכר או לא בעל הרשאות יכול להוכיח את זהותו ולקבל גישה למידע או שירותים שאחרת לא היו נגישים לו.

## איך לעשות

כדי לשלוח בקשה HTTP עם אימות בסיסי בשפת C, נצטרך להשתמש בספריה מקבילה (parallel library) ולהוסיף כותרת Base64 להודעה שנרצה לשלוח. לדוגמה:

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>

int main(void) {

  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    // יצירת מחרוזת של הכותרת הנדרשת לפי הפורמט שנדרש בבקשה
    char *auth = "Authorization: Basic xxxxx";

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, auth); // הוספת הכותרת לרשימה של הכותרות בבקשה

    curl_easy_setopt(curl, CURLOPT_URL, "http://www.example.com/"); // הכתובת של השרת שמטרתו לקבל את הבקשה
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers); // יוצא ומתחיל עם רשימת הכותרות
    res = curl_easy_perform(curl); // שליחת הבקשה
    if(res != CURLE_OK) {
      // אם הפעולה נכשלה, הדפסת הודעת שגיאה
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    }

    curl_easy_cleanup(curl); // סיום השימוש בספריה
  }
  return 0;
}
```

כאשר הקוד מוריד ומדפיס את תוצאת הבקשה לקובץ חיצוני, התוכן של הקובץ יכיל את התוכן המבוקש באמצעות האימות הבסיסי.

## צלילה עמוקה

כאשר שולחים בקשה HTTP עם אימות בסיסי, התהליך נמשך לחלוטין בהתאם לפרוטוקול התקשורת. הבקשה מכילה כותרת מסוג Authorization המכילה נתוני משתמש וסיסמה המוצגים בפורמט Base64. כאשר ה
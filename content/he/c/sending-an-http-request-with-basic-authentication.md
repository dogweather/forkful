---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# שליחת בקשת HTTP עם אימות בסיסי בשפת C

## מה זה ולמה?
שליחת בקשת HTTP עם אימות בסיסי זו תהליך שבו מסירים מידע לשרת באינטרנט, כאשר אנחנו מבקשים לפרטים מסוימים או מבצעים פעולה מסוימת. תהליך זה מצריך הזדהות - כלומר השרת מכיר אותנו ומאשר את בקשתנו.

## איך לעשות זאת:
באמצעות הספרייה `libcurl` אפשר לבצע זאת. הנה דוגמה:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
  CURL *curl = curl_easy_init();
  
  if(curl) {
    struct curl_slist *headers = NULL;
    char userpwd[64] = "username:password";
    
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
    curl_easy_setopt(curl, CURLOPT_USERPWD, userpwd);
    
    headers = curl_slist_append(headers, "custom-header: Custom-Header-Content");
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    
    CURLcode res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    else
      printf("HTTP request sent successfully\n");

    curl_easy_cleanup(curl);
  }
  
  return 0;
}
```
התוצאה תהיה: `HTTP request sent successfully`

## Deep Dive:
(1) **הקשר ההיסטורי**: שימוש בבקשות HTTP עם אימות בסיסי התפתח עם התפתחות הרשת העולמית. מאז תרמה תרומה משמעותית לשיפור תקנות האבטחה של HTTP.
(2) **חלופות**: כלכל התפתחו דרכים נוספות לאמת גולשים לשרתים, כולל אימות מרובה הצדדים, כמו OAuth.
(3) **פרטים למימוש**: במקרה שלנו, אנו משתמשים בספרייה `libcurl` כדי לבצע בקשת HTTP. הספרייה מספקת ממשק API נוח לביצוע פרוטוקולים שונים.

## ראו גם
[מדריך לחובבים של libcurl](https://curl.se/libcurl/c/libcurl-tutorial.html)

[מפתח מסמכים של libcurl](https://curl.se/libcurl/c/index.html)

תזכורת: לוודא שאתה משתמש בסיסמאות מאובטחות ומטפל בבקשות HTTP באופן מאובטח.
---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
html_title:           "C++: שליחת בקשת HTTP עם אימות בסיסי"
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## למה

## איך לעשות

כדי לשלוח בקשת HTTP עם אימות בסיסי בשפת C++, ניתן להשתמש בפונקציית `curl_easy_setopt` כדי להגדיר את הפרמטר `CURLOPT_USERPWD` עם שם המשתמש והסיסמה לאימות בסיסי. ובכדי לשלוח את הבקשה עם `curl_easy_perform` ולקבל תשובה בקוד התגובה ובגוף של התשובה.

```C++
#include <curl/curl.h>

int main()
{
    CURL *curl = curl_easy_init(); // initialize curl
    if(curl)
    {
        // set basic authentication credentials
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");
        // set endpoint URL
        curl_easy_setopt(curl, CURLOPT_URL, "https://example.com/api");
        // perform the request and get the response
        CURLcode res = curl_easy_perform(curl);
        // get response code
        long response_code;
        curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);
        // get response body
        char *response_body;
        curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_body);
        // print results
        std::cout << "Response code: " << response_code << std::endl;
        std::cout << "Response body: " << response_body << std::endl;
        // cleanup
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

## Deep Dive

כאשר משתמשים באימות בסיסי, שם המשתמש והסיסמה יתווספו להפעיל את חלק המשתמש בכותרת Authorization של הכותרת הראשית של הבקשה. זה נעשה על ידי הוספת התוים `username:password` למחרוזת המתאימה ומשלמת אותה בקידומת Base64. בכדי למצוא את כתובת הנתיב שנדרשת עבור הבקשה, עלינו לפנות לשרת הרישום או לאתר עם ההמלצות על הנתיבים המתאימים.

## See Also

- [מסמכי תיעוד של libcurl](https://curl.se/libcurl/)
- [פורומים של libcurl](https://curl.se/mail/lib-2018-04/)
- [מדריכים נוספים של C++](https://www.cplusplus.com/)
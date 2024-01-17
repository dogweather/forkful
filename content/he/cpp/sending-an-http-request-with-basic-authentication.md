---
title:                "שליחת בקשת http עם הזדהות בסיסית"
html_title:           "C++: שליחת בקשת http עם הזדהות בסיסית"
simple_title:         "שליחת בקשת http עם הזדהות בסיסית"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?

שליחת בקשת HTTP עם אימות בסיסי היא פעולה נפוצה וחשובה בתכנות. היא מאפשרת למתכנתים לשלוח בקשות לשרתים באופן מאובטח על ידי אימות זהות עם שם משתמש וסיסמה.

## איך לעשות זאת?

```c++
#include <curl/curl.h>
#include <iostream>
#include <string>

static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp)
{
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main(void)
{
    CURL *curl;
    CURLcode res;
    std::string readBuffer;
    
    curl = curl_easy_init();
    if (curl) {
        // Set the URL of the request
        curl_easy_setopt(curl, CURLOPT_URL, "https://example.com/");
        // Set the username and password for basic authentication
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");
        // Set the custom function for writing the response to a string buffer
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        // Set the pointer to the string buffer
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        
        // Perform the request
        res = curl_easy_perform(curl);
        
        // Check for errors
        if(res != CURLE_OK) {
            std::cerr << "curl_easy_perform() failed: " <<
            curl_easy_strerror(res) << std::endl;
        }
        
        // Print the response
        std::cout << readBuffer << std::endl;
        
        // Clean up
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

תוצאה דוגמאית:

```
<!DOCTYPE html>
<html>
<head>
	<title>Example Domain</title>
	<meta charset="utf-8"/>
	<meta http-equiv="Content-type" content="text/html; charset=utf-8"/>
	<meta name="viewport" content="width=device-width, initial-scale=1"/>
	<style type="text/css">
		body {
			background-color: #f0f0f2;
			margin: 0;
			padding: 0;
			font-family: -apple-system, system-ui, BlinkMacSystemFont;
		}
		div {
			width: 600px;
			margin: 5em auto;
			padding: 50px;
			background-color: #fff;
			border-radius: 1em;
		}
		a:link, a:visited {
			color: #38488f;
			text-decoration: none;
		}
		@media (max-width: 700px) {
			body {
				background-color: #fff;
			}
			div {
				width: auto;
				margin: 0 auto;
				border-radius: 0;
				padding: 1em;
			}
		}
	</style>	
</head>

<body>
<div>
	<h1>Example Domain</h1>
	<p>This domain is for use in illustrative examples in documents. You may use this
	domain in literature without prior coordination or asking for permission.</p>
	<p><a href="https://www.iana.org/domains/example">More information...</a></p>
</div>
</body>
</html>
```

## חפירה עמוקה

אימות בסיסי באמצעות שם משתמש וסיסמה הוא פרוטוקול מאוד פשוט וקל לשימוש. נוצר בכדי לתת אימות זהה בתחילת גירסת HTTP הראשונה והפך לשיטת אימות נפוצה שמשמשת לכמה מוצרים ושירותים בעתיד. אוטומציה של תפעולות שונות בטוחות שאומת יכלול טכנולוגיות יותר מאתגרות. מוצרים כמו כרטיסי אשראי ידרשו אתכם לעשות אימות יותר מסובך כדי להבטיח שלא יכתבו יותר ממה שהייתה החשיפה שאומדה, כמובן כעת שתכנות הוא מאוד חשיבה גדולה שזו הפעילות שאומבת הפעמיים. מכיוון שתיאמת הזיהום יוכלל מבחינה מספרתית, להשתמש אולי תוכלו להשיג נתונים מחדש על הפעילות המתבצעת בהרחבה.

## ראו גם

https://curl.haxx.se/libcurl/c/CURLOPT_USERPWD.html
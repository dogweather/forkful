---
title:                "שליחת בקשת http"
html_title:           "C: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מדוע

כדי לבצע פעולות באתר אינטרנט אנו צריכים לבצע בקשות HTTP. זהו תהליך חשוב וקריטי בכל פיתוח אתרים או אפליקציות ולכן חשוב לדעת כיצד לבצע בקשות HTTP בצורה נכונה.

## כיצד לבצע בקשת HTTP ב-C

התהליך המקרוי של שליחת בקשת HTTP כולל שלושה צעדים עיקריים: בניית הבקשה, שליחת הבקשה וקבלת התגובה. הנה דוגמאות של קוד C לכל אחד מהצעדים:

```
// בניית הבקשה
char *request = "GET /index.html HTTP/1.1\r\nHost: www.example.com\r\nConnection: close\r\n\r\n";

// שליחת הבקשה
int sockfd = socket(AF_INET, SOCK_STREAM, 0);
struct sockaddr_in server_addr;
server_addr.sin_family = AF_INET;
server_addr.sin_addr.s_addr = inet_addr("93.184.216.34");
server_addr.sin_port = htons(80);
connect(sockfd, (struct sockaddr *)&server_addr, sizeof(server_addr));
send(sockfd, request, strlen(request), 0);

// קבלת התגובה
char response[4096];
recv(sockfd, response, 4096, 0);
close(sockfd);

// פלט התגובה
printf("%s", response);
```

## עומק יותר על שליחת בקשת HTTP

בקשת HTTP מכילה שניים מרכיבים עיקריים: בקשה ותגובה. הבקשה מכילה את הפעולה הרצויה (GET, POST, PUT וכו'), הנתיב לעסקת (URL) וגרסת הפרוטוקול הרצויה (HTTP/1.1). כמו כן, הבקשה יכולה לכלול ראשית שליחה בשורת הכתובת שמכילה שדות נוספים כמו כותרת, פרמטרים ועוד. תגובת הבקשה מכילה את הקוד הסטטוס (200, 404 וכו'), כותרות נוספות ותוכן התגובה. חשוב לבדוק את הקוד הסטטוס לפני פעולות נוספות על התוכן המוחזר.

## ראה גם

- [The Basics of Sending an HTTP Request in C](https://www.geeksforgeeks.org/http-request-methods/) 
- [Parsing HTTP Requests in C](https://stackoverflow.com/questions/22100266/parsing-an-http-request-in-c) 
- [HTTP Requests in C - A Crash Course](https://www.ntu.edu.sg/home/ehchua/programming/webprogramming/HTTP_Basics.html)
---
title:                "C++: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה:

כשרוצים לשלוח בקשת HTTP בתכנות עם C++, ייתכן שנרצה לתקשר עם שרת או אפליקציה אחרת ולקבל מידע מעודכן מהם באופן דינמי.

## איך לבצע:

לפני שנתחיל לשלוח בקשת HTTP ב-C++, יש לוודא שכל הסביבה הנחוצה כמו ספריות ה-initiate ו-cookie מוכנות ומוכנות לשימוש. ניתן להשתמש בספריות כמו Libcurl עבור פעולות פשוטות ו-Boost.ASIO כדי לבצע פעולות מתקדמות יותר. להלן דוגמא לכתיבת פונקציה שתשלח בקשת GET לשרת ותהיה מקבלת ומדפיסה את התוצאה:

```C++
#include <iostream>
#include <boost/asio.hpp>

using boost::asio::ip::tcp;

// פונקציה לשלוח בקשת GET לשרת ולקבל ולהדפיס את התוצאה
std::string send_http_request(std::string host) {
    // יצירת רכיב הפעולה
    boost::asio::io_context io_context;

    // יצירת קליינט TCP
    tcp::resolver resolver(io_context);
    tcp::socket socket(io_context);

    // מתודת החיבור, התאמת ההתחברות ושלוח GET בקבצי Socket בקליינט באמצעות כתובת הIP המישמש על ידי השרת
    boost::asio::connect(socket, resolver.resolve(host, "http"));
    boost::asio::streambuf request;
    std::ostream request_stream(&request);
    request_stream << "GET / HTTP/1.0\r\n";
    request_stream << "Host: " << host << "\r\n";
    request_stream << "Accept: */*\r\n";
    request_stream << "Connection: close\r\n\r\n";

    // כתיבת בקשה לשרת
    boost::asio::write(socket, request);

    // קריאה והדפסת תוצאה מחזרת מהשרת
    boost::asio::streambuf response;
    boost::asio::read_until(socket, response, "\r\n");
    std::string result;
    std::istream response_stream(&response);
    while (!response_stream.eof()) {
        std::getline(response_stream, result);
    }

    return result;
}

// פונקציה עיקרית להרצת הקוד ובדיקת פעולתו
int main() {
    std::cout << "Send HTTP request..." << std::endl;
    std::string result = send_http_request("www.example.com");
    std::cout << "Response from server: " << std::endl;
    std::cout << result << std::endl;
    return 0;
}
```

## הכנסה למים בעומק:

שליחת בקשת HTTP ב-C++ נעשית על ידי יצירת חיבור TCP וש
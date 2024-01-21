---
title:                "שליחת בקשת HTTP"
date:                  2024-01-20T18:00:05.576749-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא פנייה מכוונת מקוד לשרת באינטרנט, כאילו אומרים: "היי, תן לי איזשהו מידע או לך תעשה משהו". מתכנתים עושים את זה כדי לקבל מידע ממרחבי האינטרנט או לדחוף לשם נתונים - מבצעים שימוש ב-APIs, מזמינים משאבים ועושים אינטרקציות עם אתרי אינטרנט.

## איך לעשות:
```C++
#include <iostream>
#include <cpprest/http_client.h>
#include <cpprest/filestream.h>

int main() {
    auto fileStream = std::make_shared<concurrency::streams::ostream>();
    // Open stream to output file.
    pplx::task<void> requestTask = concurrency::streams::fstream::open_ostream(U("results.html"))

    .then([=](concurrency::streams::ostream outFile) {
        *fileStream = outFile;

        // Create http_client to send the request.
        web::http::client::http_client client(U("http://www.example.com"));

        // Build request URI and start the request.
        uri_builder builder(U("/"));
        return client.request(web::http::methods::GET, builder.to_string());
    })

    // Handle response headers arriving.
    .then([=](web::http::http_response response) {
        printf("Received response status code:%u\n", response.status_code());
        return response.body().read_to_end(fileStream->streambuf());
    })

    // Close the file stream.
    .then([=](size_t) {
        return fileStream->close();
    });

    // Wait for all the outstanding I/O to complete and handle any exceptions
    try {
        requestTask.wait();
    }
    catch (const std::exception &e) {
        printf("Error exception:%s\n", e.what());
    }

    return 0;
}
```
תוצאת הדוגמא לעיל: היא קובץ בשם `results.html` שבתוכו יש את תוכן הדף מהאתר `www.example.com`.

## צלילה לעומק:
לשליחת בקשות HTTP מ-C++ יש היסטוריה של שימוש בספריות כמו libcurl או Qt's Network module. אלה עדיין חלופות טובות, אבל חבילת C++ Rest SDK כוללת את cpprest שמספקת ממשק נקי ומודרני יותר. זה משתמש בתכנות אסינכרוני מודרני של C++11 ומעלה, כולל מתנדים (futures) ומשימות (tasks) לקבלת תוצאות בתהליך אסינכרוני, מה שמפחית את ההתעסקות עם חוטי Thread מסובכים.

## ראו גם:
- דוקומנטציה של C++ Rest SDK: https://github.com/microsoft/cpprestsdk
- תיעוד של libcurl (אלטרנטיבה נפוצה): https://curl.se/libcurl/
- מדריכים להשתמש ב-QT Network module: https://doc.qt.io/qt-5/qtnetwork-index.html

כשבא לכם להתחיל לעבוד עם שליחת בקשות HTTP ב-C++, שקלו באיזה סוג של פרויקט אתם עובדים ובחרו בספרייה שמתאימה ביותר לצרכים שלכם.
---
date: 2024-01-20 18:00:05.576749-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D4\u05D9\
  \u05D0 \u05E4\u05E0\u05D9\u05D9\u05D4 \u05DE\u05DB\u05D5\u05D5\u05E0\u05EA \u05DE\
  \u05E7\u05D5\u05D3 \u05DC\u05E9\u05E8\u05EA \u05D1\u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8, \u05DB\u05D0\u05D9\u05DC\u05D5 \u05D0\u05D5\u05DE\u05E8\u05D9\u05DD\
  : \"\u05D4\u05D9\u05D9, \u05EA\u05DF \u05DC\u05D9 \u05D0\u05D9\u05D6\u05E9\u05D4\
  \u05D5 \u05DE\u05D9\u05D3\u05E2 \u05D0\u05D5 \u05DC\u05DA \u05EA\u05E2\u05E9\u05D4\
  \ \u05DE\u05E9\u05D4\u05D5\". \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\
  \u05DC \u05DE\u05D9\u05D3\u05E2 \u05DE\u05DE\u05E8\u05D7\u05D1\u05D9\u2026"
lastmod: 2024-02-19 22:04:59.093145
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D4\u05D9\
  \u05D0 \u05E4\u05E0\u05D9\u05D9\u05D4 \u05DE\u05DB\u05D5\u05D5\u05E0\u05EA \u05DE\
  \u05E7\u05D5\u05D3 \u05DC\u05E9\u05E8\u05EA \u05D1\u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8, \u05DB\u05D0\u05D9\u05DC\u05D5 \u05D0\u05D5\u05DE\u05E8\u05D9\u05DD\
  : \"\u05D4\u05D9\u05D9, \u05EA\u05DF \u05DC\u05D9 \u05D0\u05D9\u05D6\u05E9\u05D4\
  \u05D5 \u05DE\u05D9\u05D3\u05E2 \u05D0\u05D5 \u05DC\u05DA \u05EA\u05E2\u05E9\u05D4\
  \ \u05DE\u05E9\u05D4\u05D5\". \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\
  \u05DC \u05DE\u05D9\u05D3\u05E2 \u05DE\u05DE\u05E8\u05D7\u05D1\u05D9\u2026"
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
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

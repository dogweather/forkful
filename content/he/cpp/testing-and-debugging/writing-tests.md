---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:52.575774-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA\
  \ \u05D1-C++ \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA \u05EA\
  \u05D5\u05DB\u05E0\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA, \u05E2\u05E6\
  \u05DE\u05D0\u05D9\u05D5\u05EA, \u05E9\u05D1\u05D5\u05D3\u05E7\u05D5\u05EA \u05D1\
  \u05D0\u05D5\u05E4\u05DF \u05D0\u05D5\u05D8\u05D5\u05DE\u05D8\u05D9 \u05D0\u05EA\
  \ \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\u05EA \u05E9\u05DC \u05E7\u05D8\u05E2\u05D9\
  \u05DD \u05D1\u05D1\u05E1\u05D9\u05E1 \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA\
  . \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D5\u05D5\u05D3\u05D0 \u05E9\u05D4\u05E7\
  \u05D5\u05D3 \u05E9\u05DC\u05D4\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.841795-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05D1\
  -C++ \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA \u05EA\u05D5\u05DB\
  \u05E0\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA, \u05E2\u05E6\u05DE\u05D0\
  \u05D9\u05D5\u05EA, \u05E9\u05D1\u05D5\u05D3\u05E7\u05D5\u05EA \u05D1\u05D0\u05D5\
  \u05E4\u05DF \u05D0\u05D5\u05D8\u05D5\u05DE\u05D8\u05D9 \u05D0\u05EA \u05D4\u05EA\
  \u05E0\u05D4\u05D2\u05D5\u05EA \u05E9\u05DC \u05E7\u05D8\u05E2\u05D9\u05DD \u05D1\
  \u05D1\u05E1\u05D9\u05E1 \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D5\u05D5\u05D3\u05D0 \u05E9\u05D4\u05E7\u05D5\u05D3\
  \ \u05E9\u05DC\u05D4\u05DD\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

כתיבת בדיקות ב-C++ כוללת יצירת תוכניות קטנות, עצמאיות, שבודקות באופן אוטומטי את התנהגות של קטעים בבסיס הקוד שלך. מתכנתים עושים זאת כדי לוודא שהקוד שלהם עובד כצפוי, למנוע רגרסיות (כלומר, שינויים חדשים שמשבשים פונקציונליות קיימת), ולסייע בתחזוקת בסיסי קוד לאורך זמן.

## איך לעשות:

### באמצעות Google Test Framework

אחת מספריות הצד השלישי הפופולריות ביותר לכתיבת בדיקות ב-C++ היא Google Test. תחילה, תצטרך להתקין את Google Test ולקשר אותו עם הפרויקט שלך. לאחר ההקמה, תוכל להתחיל לכתוב מקרי בדיקה.

```cpp
#include <gtest/gtest.h>

int add(int a, int b) {
    return a + b;
}

TEST(TestSuiteName, TestName) {
    EXPECT_EQ(3, add(1, 2));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

שמור את הקוד בקובץ, ותקמפל אותו עם המהדר g++, תוך קישור הספרייה של Google Test. אם הכל הוקם כראוי, הרצת הביצוע המתקבל תריץ את הבדיקה, ואם הפונקציה `add` פועלת כצפוי, תראה משהו כמו:

```
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from TestSuiteName
[ RUN      ] TestSuiteName.TestName
[       OK ] TestSuiteName.TestName (0 ms)
[----------] 1 test from TestSuiteName (0 ms total)

[==========] 1 test from 1 test suite ran. (1 ms total)
[  PASSED  ] 1 test.
```

### באמצעות Catch2 

מסגרת בדיקה פופולרית נוספת ל-C++ היא Catch2. יש לה תחביר פשוט יותר ולרוב לא דורשת קישור נגד ספרייה (רק כותרת). להלן דוגמה איך לכתוב בדיקה פשוטה עם Catch2:

```cpp
#define CATCH_CONFIG_MAIN  // זה אומר ל-Catch לספק main() - עשה זאת רק בקובץ cpp אחד
#include <catch.hpp>

int multiply(int a, int b) {
    return a * b;
}

TEST_CASE( "מכפלת מספרים שלמים", "[multiply]" ) {
    REQUIRE( multiply(2, 3) == 6 );
}
```

לאחר הקמפול והרצת הבדיקה הזו, Catch2 מספקת פלט ברור המציין אם הבדיקה עברה או נכשלה, יחד עם כל מידע נדרש לאיתור בעיות:

```
===============================================================================
כל הבדיקות עברו (1 טענה במקרה בדיקה אחד)
```

הדוגמאות הללו מראות איך אינטגרציה של מסגרות בדיקה לתהליך פיתוח ה-C++ שלך יכולה לשפר באופן משמעותי את האמינות והתחזוקתיות של הקוד שלך.

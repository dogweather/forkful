---
date: 2024-01-20 18:03:45.918889-07:00
description: "\u05DB\u05E9\u05DE\u05EA\u05D7\u05D9\u05DC\u05D9\u05DD \u05E4\u05E8\u05D5\
  \u05D9\u05E7\u05D8 \u05D7\u05D3\u05E9 \u05D1-Clojure, \u05D0\u05E0\u05D7\u05E0\u05D5\
  \ \u05D1\u05E2\u05E6\u05DD \u05D9\u05D5\u05E6\u05E8\u05D9\u05DD \u05E1\u05D1\u05D9\
  \u05D1\u05D4 \u05E0\u05E7\u05D9\u05D9\u05D4 \u05E2\u05D1\u05D5\u05E8 \u05E7\u05D5\
  \u05D3 \u05E9\u05E2\u05EA\u05D9\u05D3 \u05DC\u05D2\u05D3\u05D5\u05DC. \u05EA\u05D5\
  \u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D1\u05D8\u05D9\u05D7 \u05D4\u05EA\
  \u05D7\u05DC\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05D5\u05DE\u05E1\u05D5\
  \u05D3\u05E8\u05EA \u05E9\u05DE\u05E9\u05EA\u05DC\u05D1\u05EA \u05E2\u05DD \u05EA\
  \u05D4\u05DC\u05D9\u05DB\u05D9\u2026"
lastmod: 2024-02-19 22:04:57.976764
model: gpt-4-1106-preview
summary: "\u05DB\u05E9\u05DE\u05EA\u05D7\u05D9\u05DC\u05D9\u05DD \u05E4\u05E8\u05D5\
  \u05D9\u05E7\u05D8 \u05D7\u05D3\u05E9 \u05D1-Clojure, \u05D0\u05E0\u05D7\u05E0\u05D5\
  \ \u05D1\u05E2\u05E6\u05DD \u05D9\u05D5\u05E6\u05E8\u05D9\u05DD \u05E1\u05D1\u05D9\
  \u05D1\u05D4 \u05E0\u05E7\u05D9\u05D9\u05D4 \u05E2\u05D1\u05D5\u05E8 \u05E7\u05D5\
  \u05D3 \u05E9\u05E2\u05EA\u05D9\u05D3 \u05DC\u05D2\u05D3\u05D5\u05DC. \u05EA\u05D5\
  \u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D1\u05D8\u05D9\u05D7 \u05D4\u05EA\
  \u05D7\u05DC\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05D5\u05DE\u05E1\u05D5\
  \u05D3\u05E8\u05EA \u05E9\u05DE\u05E9\u05EA\u05DC\u05D1\u05EA \u05E2\u05DD \u05EA\
  \u05D4\u05DC\u05D9\u05DB\u05D9\u2026"
title: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9"
---

{{< edit_this_page >}}

## מה ולמה?
כשמתחילים פרויקט חדש ב-Clojure, אנחנו בעצם יוצרים סביבה נקייה עבור קוד שעתיד לגדול. תוכניתנים עושים זאת כדי להבטיח התחלה מובנית ומסודרת שמשתלבת עם תהליכי עבודה וכלים מודרניים.

## איך לעשות:
כדי להתחיל פרויקט חדש ב-Clojure, בואו נשתמש ב-Leiningen, הכלי הפופולארי ביותר עבור ניהול פרויקטים ב-Clojure.

התקינו את Leiningen:

```shell
brew install leiningen
```

לאחר ההתקנה, צרו פרויקט חדש:

```shell
lein new app my-cool-project
```

פלט הדוגמה:

```text
Generating a project called my-cool-project based on the 'app' template.
```

דירוגטוריית הפרויקט החדש שלכם צריכה להיראות כך:

```shell
tree my-cool-project
```

```text
my-cool-project
├── project.clj
├── README.md
├── resources
├── src
│   └── my_cool_project
│       └── core.clj
└── test
    └── my_cool_project
        └── core_test.clj
```

כעת, אתם מוכנים להתחיל לכתוב קוד.

## שיקולים מעמיקים:
Leiningen היה הכלי המהפכני ששינה את דרך עבודתם של מפתחים ב-Clojure. פותח ב-2009, זה הפך לסטנדרט דה פקטו לניהול פרויקטים ב-Clojure. חלק מהאלטרנטיבות כוללות כלים כמו Boot ו-Clojure CLI, אך כיום Leiningen עדיין הוא המועדף עבור הרבה מפתחים. בעוד ש-Leiningen מאפשר לנו ליצור פרויקטים, לנהל תלויות ולהפעיל משימות, חשוב להיות מודעים למאפייני פרויקט מודרניים - מערכת בנייה גמישה, טיפול בסביבות שונות וקוד שיכול לספק ביצועים טובים.

## ראו גם:
- [Leiningen's Official Website](http://leiningen.org/)
- [Clojure for the Brave and True - Starting a Project](https://www.braveclojure.com/getting-started/)
- [Clojure CLI - an alternative tool to Leiningen](https://clojure.org/guides/deps_and_cli)

---
date: 2024-01-20 18:03:45.918889-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DB\u05D3\u05D9\
  \ \u05DC\u05D4\u05EA\u05D7\u05D9\u05DC \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9 \u05D1-Clojure, \u05D1\u05D5\u05D0\u05D5 \u05E0\u05E9\u05EA\u05DE\u05E9\
  \ \u05D1-Leiningen, \u05D4\u05DB\u05DC\u05D9 \u05D4\u05E4\u05D5\u05E4\u05D5\u05DC\
  \u05D0\u05E8\u05D9 \u05D1\u05D9\u05D5\u05EA\u05E8 \u05E2\u05D1\u05D5\u05E8 \u05E0\
  \u05D9\u05D4\u05D5\u05DC \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8\u05D9\u05DD \u05D1\
  -Clojure. \u05D4\u05EA\u05E7\u05D9\u05E0\u05D5 \u05D0\u05EA Leiningen."
lastmod: '2024-03-13T22:44:38.705453-06:00'
model: gpt-4-1106-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05D7\u05D9\u05DC \u05E4\u05E8\u05D5\
  \u05D9\u05E7\u05D8 \u05D7\u05D3\u05E9 \u05D1-Clojure, \u05D1\u05D5\u05D0\u05D5 \u05E0\
  \u05E9\u05EA\u05DE\u05E9 \u05D1-Leiningen, \u05D4\u05DB\u05DC\u05D9 \u05D4\u05E4\
  \u05D5\u05E4\u05D5\u05DC\u05D0\u05E8\u05D9 \u05D1\u05D9\u05D5\u05EA\u05E8 \u05E2\
  \u05D1\u05D5\u05E8 \u05E0\u05D9\u05D4\u05D5\u05DC \u05E4\u05E8\u05D5\u05D9\u05E7\
  \u05D8\u05D9\u05DD \u05D1-Clojure."
title: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9"
weight: 1
---

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

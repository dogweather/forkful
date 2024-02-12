---
title:                "התחלת פרויקט חדש"
aliases:
- /he/clojure/starting-a-new-project/
date:                  2024-01-20T18:03:45.918889-07:00
model:                 gpt-4-1106-preview
simple_title:         "התחלת פרויקט חדש"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/clojure/starting-a-new-project.md"
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

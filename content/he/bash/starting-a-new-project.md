---
title:                "התחלת פרויקט חדש"
html_title:           "Clojure: התחלת פרויקט חדש"
simple_title:         "התחלת פרויקט חדש"
programming_language: "Bash"
category:             "Bash"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה ולמה?

בתחילת פרויקט חדש אנחנו מגדירים משימה ממוחשבת שאנחנו רוצים לבצע. מתכנתים עושים את זה כדי ליצור פתרון יעיל לבעיה מסוימת.

## איך לעשות:

```Bash
# שלב ראשון: יצירת ספרייה חדשה
mkdir my_new_project
cd my_new_project

# שלב שני: התקנת הכלים הנדרשים
sudo apt-get install git
sudo apt-get install nodejs

# שלב שלישי: יצירת קובץ README
echo "# My New Project" > README.md
```

## צלילה מעמיקה:

1. ההיסטוריה: Bash הוא שפת תכנות שנוצרה בשנים ה-80. היא בעיקר משמשת לניהול משימות מנגנון ההפעלה.
2. אלטרנטיבות: Python או Ruby יכולים להיות אלטרנטיבות טובות לסביבת Bash.
3. פרטים על המימוש: "mkdir" מייצרת ספרייה חדשה, "cd" משנה את ספריית העבודה, "sudo apt-get install" מתקינה תוכנות, אילו "echo" מייצרת קובץ חדש.

## ראה גם:

1. מדריך Bash למתחילים: https://he.wikibooks.org/wiki/Bash
2. קורס בינוני ב-Bash: https://www.codecademy.com/learn/learn-the-command-line
3. Bash למתקדמים: https://tldp.org/LDP/abs/html/
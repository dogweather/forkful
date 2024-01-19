---
title:                "התחלת פרויקט חדש"
html_title:           "Clojure: התחלת פרויקט חדש"
simple_title:         "התחלת פרויקט חדש"
programming_language: "Python"
category:             "Python"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/starting-a-new-project.md"
---

{{< edit_this_page >}}

# מה זה ולמה? 
כאשר אנו מתחילים פרויקט חדש, אנו גורסים מסגרת תכנות חדשה מאפס. אנו עושים זאת כדי להתמקד בבעיה ספציפית, ליישם רעיון חדש או לייצר מוצר.

# איך לעשות:
חלק מהתהליך של יצירת קוד חדש משתמש במערכות פיתוח, כמו GIT, במקרה זה, אליו אני מתייחס. 
```
# Python
import os
os.system('git init')
os.system('echo "# MyProject" >> README.md')
os.system('git add README.md')
os.system('git commit -m "first commit"')
os.system('git branch -M main')
os.system('git remote add origin https://github.com/user/repo.git')
os.system('git push -u origin main')
```
התוצאה היא יצירת מאגר חדש ב-Git, עם commit ראשון המוסיף קובץ README.

# Deep Dive:
במשך השנים, נהוג להתחיל פרויקטים חדשים מבלי שהם מתוך קוד קיים. זמינות של מערכת פיתוח כמו Git הפכה את התהליך לממוחשב ומנוהל. אם אנו מחפשים חלופה ל- Git, אנו יכולים לבחור ב SVN, Mercurial או אפילו CVS. כמו שאנו רואים בדוגמה למעלה, אנו משתמשים במספר פעולות ליצירת סביבה חדשה לפרויקט.

# ראה גם:
- המדריך למשתמש של GIT: https://git-scm.com/doc
- קישור לדוקומנטציה של Python: https://docs.python.org/3/
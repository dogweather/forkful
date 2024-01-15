---
title:                "הורדת דף אינטרנט"
html_title:           "Bash: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## למה

מדוע ישנם רבים שמתעסקים בהורדת דף אינטרנט? הפתרון הוא פשוט - כי זה דרך מצוינת לגשת למידע מכל מקום ובכל זמן רק באמצעות מחשוב וחיבור לאינטרנט.

## איך לעשות זאת

הורדת דף אינטרנט באמצעות Bash הינה פשוטה מאוד. כדי לעשות זאת, יש להשתמש בפקודת `wget` ולציין את הכתובת האתר שברצונכם להוריד. לדוגמה:

```Bash
wget https://www.example.com
```

אם תזינו את הפקודה הנ"ל, האתר יורד למחשב שלכם וישמר כקובץ בשם `index.html` בתיקיית העבודה הנוכחית. תוכלו לשנות את השם הוצאת הקובץ ע"י הוספת שם משתנה לפקודה `wget`. לדוגמה:

```Bash
wget -O mypage.html https://www.example.com
```

כעת הדף יורד עם השם `mypage.html` והוא יישמר בתיקיית העבודה הנוכחית.

## העמקה נוספת

הורדת דף אינטרנט עם Bash הינה פעולה נוחה ומהירה כאשר כתובת האתר ידועה. אך כאשר מדובר באתרים גדולים יותר, כמו אתרי חדשות או בלוגים שמעדכנים תדירות, מומלץ להשתמש בכלי ניהול הוראות HTTP כמו `curl` או `wget` עם אפשרויות נוספות כדי לקבל מידע מתוחם.

## ראו גם

למידע נוסף על Bash ושימושים נוספים שניתן לעשות עם קוד ההפקודה, בקרו בקישורים הבאים:

- מדריכים ופעולות בסיסיות: https://www.gnu.org/software/bash/manual/
- דברים מעניינים שניתן לעשות עם Bash: https://www.linuxjournal.com/content/using-bash
- ביצוע כתף בכתפיים באמצעות Bash: https://www.howtogeek.com/howto/29980/what-is-bash-and-why-should-i-use-it/
---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

---

# הורדת דף אינטרנט באמצעות Bash

---

## מה זה ולמה?

הפעולה שנקראת "הורדת דף אינטרנט" היא בעצם שליחת בקשת GET לשרת באמצעות HTTP. התכנתים בוחרים להוריד דף אינטרנט כדי לבצע ניתוח נתונים, לבדוק זמינות של אתר או לגרום לפעולות אוטומטיות להתרחש.

---

## כיצד:

```Bash
wget http://example.com
```

פלט דוגמה:

```Bash
--2021-05-06 20:04:34--  http://example.com/
Resolving example.com (example.com)... 93.184.216.34, 2606:2800:220:1:248:1893:25c8:1946
Connecting to example.com (example.com)|93.184.216.34|:80... connected.
HTTP request sent, awaiting response... 200 OK
Length: 1256 (1.2K) [text/html]
Saving to: ‘index.html’

index.html          100%[===================>]   1.23K  --.-KB/s    in 0s      

2021-05-06 20:04:34 (156 MB/s) - ‘index.html’ saved [1256/1256]
```

---

## העמקת הפרטים

*בהקשר היסטורי:* קומנדה `wget` אותה אנו משתמשים היום נכתבה למעלה מ-20 שנה. האמצעים האינטרנטיים השתנו מאז, אך המחשבים עדיין משתמשים ב-`wget` לשליחת בקשות HTTP. 

*אלטרנטיבות:* ל-`wget` קיימות אלטרנטיבות רבות, כמו `curl` ו-`HTTPie`. כל אחת מהן מבצעת את המשימה בשיטות שונות ומציגה פלט שונה.

*פרטי המימוש:* ברוב המקרים, `wget` ישמור את הדף המבוקש בקובץ `index.html`. הדף נשמר כקובץ HTML פשוט, כך שניתן לפתוח אותו ולנתחו באמצעות Bash או שפות תכנות אחרות.

---

## מקורות נוספים

* מדריך של ה-`Curl`: [קישור](https://curl.haxx.se/docs/httpscripting.html)
* מדריך של ה-`HTTPie`: [קישור](https://httpie.org/doc)
* הסבר מעמיק על בקשות HTTP: [קישור](https://www.tutorialspoint.com/http/http_requests.htm)
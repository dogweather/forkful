---
title:                "ניתוח HTML"
date:                  2024-01-20T15:30:37.102213-07:00
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
פירוס HTML הוא תהליך שבו אנו קוראים ומנתחים מבנה ה-HTML של עמוד ווב כדי להשיג מידע ספציפי ממנו. תכנתים עושים זאת כדי לאוטומט עיבוד נתונים, לקחת נתונים מאתרים ללא API, או לבצע בדיקות אוטומטיות על הממשק החזותי.

## איך לעשות:
השתמשו בכלים כמו `curl` לשליפת דפי HTML ובכלים כמו `grep`, `awk`, ו `sed` לפירוס וחיפוש נתונים. עדיף להימנע מלעשות פירוס ידני ולהשתמש בספריות כמו `pup` או `html-xml-utils` שמקלות על העבודה:

```Bash
# קבלת ה-HTML של אתר ושמירתו בקובץ
curl 'http://example.com' > example.html

# פירוס תגית כותרת מהקובץ שהתקבל
grep -oP '(?<=<title>).*?(?=</title>)' example.html

# שימוש ב-pup לקבלת טקסט בתוך פסקה מסויימת
pup 'p.some-class text{}' < example.html
```

פלט הדוגמא:
```
Title of Example Domain
The text inside the paragraph with 'some-class' class.
```

## עיון מעמיק:
פירוס HTML היה חלק מההיסטוריה של האינטרנט עוד לפני ש- APIs הפכו לנפוצים. בימים הראשונים, הרבה נתונים נמשכו מדפים באינטרנט באמצעות פירוס ידני שאינו אידיאלי ויכול להתדרדר עם שינויים במבנה הדף. כיום יש ספריות עשירות כמו Beautiful Soup ב-Python או Nokogiri ב-Ruby שמספקות API עשיר וקל לשימוש לפירוס HTML. ב-Bash, ספריות כמו `pup` ו-`html-xml-utils` מוותרות על הצורך בפירוס ידני ומאפשרות עבודה מורכבת יותר על מבנה ה-HTML. חשוב לזכור שפירוס HTML באופן אוטומטי יכול להפר תנאי שימוש של אתר, לכן תמיד צריך לבדוק את המדיניות של האתר לפני שמתחילים.

## ראו גם:
- [pup](https://github.com/ericchiang/pup): כלי קונסולת Bash לפירוס וטיפול בקבצי HTML.
- [html-xml-utils](https://www.w3.org/Tools/HTML-XML-utils): קבוצת כלים שפותחו על ידי W3C לעבודה עם HTML ו XML.
- [Beautiful Soup Documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/): מידע על ספריית Beautiful Soup של Python, פופולרית לפירוס HTML.
- [Nokogiri Website](https://nokogiri.org/): האתר הרשמי של Nokogiri, ספריה לפירוס HTML וXML ב-Ruby.
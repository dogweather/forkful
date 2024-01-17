---
title:                "פירוק html"
html_title:           "Bash: פירוק html"
simple_title:         "פירוק html"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
לימוד הנדסת HTML הינו תהליך שבו מתחילים לקרוא ולנתח קוד מקור של דף אינטרנט כדי להבין את מבנהו ולקלוט אותו כמחרוזת שניתן לעבוד איתה. הדבר חשוב במיוחד למתכנתים על מנת לייעל את תהליך הפיתוח וליצור אפליקציות ואתרים ברמה גבוהה.

## איך לעשות זאת?
כאשר עבודה עם מחרוזות של HTML, ניתן להשתמש בכלים שונים כדי לפענח את הקוד המקורי. לדוגמה, נוכל להשתמש בפקודות של Bash כדי להדפיס את התוכן של דף אינטרנט או להפוך אותו לקובץ טקסט. ניתן גם להשתמש בתוכנות נבנות לפענוח קידוד של הקוד המקורי ולהציג אותו בצורה ברורה יותר.

```Bash
# הצגת תוכן האתר כחלק מהפקודות של Bash:
curl -s https://www.example.com | grep "<title>"

# המרת האתר לקובץ טקסט עם תוכן נקי:
wget -qO- https://www.example.com | lynx -stdin -dump -nolist

``` 

## חקירה מעמיקה
לימוד תהליך הנדסת HTML הינו חשוב במיוחד בעולם המתכנתים עם ניסיון. בעבר, לימוד הנדסת HTML היה בעיקר מופשט למתכנתים ניידים בלבד. כיום, ישנן תוכניות נבנות שניתן להשתמש בהן על מנת לקלוט קוד מקורי ולהציג את מבנהו כ- DOM תקין. אופציות אחרות כוללות גם שימוש בתכניות לניתוק קידוד של קוד מקור והפיכתו לקל לקריאה על ידי משתמשים אחרים. יתר על כן, בכדי להציג את התוצאות כחומר למידה שימושי, ניתן לנקות ולטפל בתגים וכו'.

## ראו גם
- [כיצד ללמוד HTML כמתחיל](https://www.lifewire.com/learn-html-for-beginners-3466603)
- [מה זה DOM?](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Introduction)
- [ספריות צד שלישי לניתוח קוד HTML](https://www.sitespeed.io/doc/Third-party-analyzers/)
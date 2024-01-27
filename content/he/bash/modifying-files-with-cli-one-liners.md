---
title:                "שינוי קבצים בשורת הפקודה באמצעות ביטוי אחד"
date:                  2024-01-26T22:20:38.743622-07:00
model:                 gpt-4-0125-preview
simple_title:         "שינוי קבצים בשורת הפקודה באמצעות ביטוי אחד"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/modifying-files-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## מה ולמה?

לשנות קבצים באמצעות שורות פקודה בודדות ב-CLI (ממשק שורת הפקודה) זה כל העניין בלערוך שינויים מהירים וממוקדים בקבצים ישירות מהטרמינל שלך. תכנתים עושים את זה מכיוון שזה מהיר, ניתן לתסריטים, וכאשר עובדים בסביבות כמו Linux, זה לעיתים הדרך הכי ישירה להחיל שינויים בלי לפתוח עורך אמיתי. זה מנצל את הכוח של sed, awk, grep, וכלים נוספים בשורת הפקודה כדי לחפש, להחליף, להוסיף או למחוק תוכן קבצים במהלך העבודה.

## איך ל:

בואו נעבור על כמה דוגמאות בסיסיות:

1.**החלפת טקסט** בקובץ באמצעות `sed`:
   ```Bash
   sed -i 's/oldText/newText/g' filename.txt
   ```
   פקודה זו מחפשת את `oldText` ב-`filename.txt` ומחליפה אותו ב-`newText`.

2.**הוספת טקסט** לקובץ:
   ```Bash
   echo "New line of text" >> filename.txt
   ```
   מוסיפה שורת טקסט חדשה לסוף `filename.txt`.

3.**מחיקת שורה** המכילה מחרוזת מסוימת עם `sed`:
   ```Bash
   sed -i '/stringToDelete/d' filename.txt
   ```
   מוחקת שורות המכילות את `stringToDelete` מ-`filename.txt`.

4.**חילוץ והדפסה** של שורות התואמות לתבנית באמצעות `grep`:
   ```Bash
   grep 'patternToMatch' filename.txt
   ```
   מציגה שורות מ-`filename.txt` התואמות לתבנית.

## עיון נוסף

לשנות קבצים באמצעות שורות פקודה בודדות היא טכניקה כמו יוניקס עצמו, התלויה במידה רבה בכלים כמו `sed`, `awk`, `grep`, ו-`cut`. כלים אלו תוכננו בימי יוניקס הראשונים כדי לטפל במשימות עיבוד טקסט ביעילות, תוך שימוש במושג הצינור המהפכני אז.

**חלופות**: למרות ששורות פקודה אלו חזקות, יש להן מגבלות, במיוחד בעת עיסוק במבני נתונים מורכבים יותר או בקבצי בינאריים. במקרים כאלו, שפות תסריט רמה גבוהה יותר כמו Python או Perl עשויות להיות מתאימות יותר בשל יכולות הניתוח והניהול של הנתונים המתקדמות שלהן.

**פרטי יישום**: הבנת ביטויים רגולריים (regex) היא קריטית בעת עבודה עם כלים אלה, מאחר שהם אבן היסוד בהתאמת תבניות ובעיבוד טקסט. יתרה מזאת, האופציה `-i` עם `sed` לעריכה במקום לא פועלת באופן אחיד בכל המערכות באותה הדרך, במיוחד ב-macOS לעומת Linux, שם ייתכן ותצטרך לכלול ארגומנט לסיומת גיבוי עם `-i` ב-macOS.

## ראה גם

- מדריך ל-GNU `sed`: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
- שפת התכנות AWK: [https://www.cs.princeton.edu/~bwk/btl.mirror/](https://www.cs.princeton.edu/~bwk/btl.mirror/)
- דף מדריך ל-Grep: [https://www.gnu.org/software/grep/manual/grep.html](https://www.gnu.org/software/grep/manual/grep.html)
- מידע על ביטויים רגולריים: [https://www.regular-expressions.info/](https://www.regular-expressions.info/)

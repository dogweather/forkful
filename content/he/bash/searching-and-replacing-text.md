---
title:                "חיפוש והחלפת טקסט"
date:                  2024-01-20T17:57:13.491003-07:00
model:                 gpt-4-1106-preview
simple_title:         "חיפוש והחלפת טקסט"

category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיפוש והחלפת טקסט הוא פעולה שבה מוצאים רצף תווים בתיקסט ומחליפים אותו ברצף אחר. תכנתי עושים את זה כדי לתקן שגיאות, לעדכן מידע או לשנות פורמט.

## איך לעשות:
```Bash
# חיפוש והחלפה בקובץ באמצעות sed:
sed -i 's/old_text/new_text/g' filename.txt

# דוגמא לקלט:
echo "Welcome to Bash programming!" > welcome.txt
sed -i 's/Bash/Shell/g' welcome.txt
cat welcome.txt

# דוגמא לפלט:
Shell programming!
```

## עיון מעמיק
החלפת טקסט היא אחת מהפעולות הבסיסיות בעיבוד טקסטים שרצה כבר מימי UNIX הראשונים. הכלי `sed` (stream editor) משמש לעיבוד טקסט בזרימה והוא נשאר בשימוש נרחב עד היום. ישנם חלופות כמו `awk`, המתמחה גם בעיבוד שורות ועמודות, וכלים מודרניים כמו `perl` ו`python`, אשר מספקים יכולות מתקדמות יותר לביטויים רגולריים ועיבוד טקסט. בנוגע להחלפה עצמה, הפקודה `s/old_text/new_text/g` שבדוגמה מבצעת חיפוש אחר הביטוי "old_text" והחלפתו ב"new_text", כאשר הדגל `g` מציין החלפה גלובלית בכל המופעים בטקסט ולא רק בראשון שנמצא.

## ראו גם:
- [GNU sed manual](https://www.gnu.org/software/sed/manual/sed.html) - מדריך ל-kn`sed`
- [Regular Expressions Info](https://www.regular-expressions.info/) - משאב ללמידת ביטויים רגולריים
- [AWK User's Guide](https://www.gnu.org/software/gawk/manual/gawk.html) - מדריך למשתמש של `awk`

---
date: 2024-01-20 17:57:13.491003-07:00
description: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA\
  \ \u05D8\u05E7\u05E1\u05D8 \u05D4\u05D5\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\
  \u05D1\u05D4 \u05DE\u05D5\u05E6\u05D0\u05D9\u05DD \u05E8\u05E6\u05E3 \u05EA\u05D5\
  \u05D5\u05D9\u05DD \u05D1\u05EA\u05D9\u05E7\u05E1\u05D8 \u05D5\u05DE\u05D7\u05DC\
  \u05D9\u05E4\u05D9\u05DD \u05D0\u05D5\u05EA\u05D5 \u05D1\u05E8\u05E6\u05E3 \u05D0\
  \u05D7\u05E8. \u05EA\u05DB\u05E0\u05EA\u05D9 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\
  \u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05EA\u05E7\u05DF \u05E9\u05D2\u05D9\
  \u05D0\u05D5\u05EA, \u05DC\u05E2\u05D3\u05DB\u05DF \u05DE\u05D9\u05D3\u05E2 \u05D0\
  \u05D5 \u05DC\u05E9\u05E0\u05D5\u05EA \u05E4\u05D5\u05E8\u05DE\u05D8."
lastmod: '2024-03-13T22:44:39.596000-06:00'
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8 \u05D4\u05D5\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\
  \u05D4 \u05DE\u05D5\u05E6\u05D0\u05D9\u05DD \u05E8\u05E6\u05E3 \u05EA\u05D5\u05D5\
  \u05D9\u05DD \u05D1\u05EA\u05D9\u05E7\u05E1\u05D8 \u05D5\u05DE\u05D7\u05DC\u05D9\
  \u05E4\u05D9\u05DD \u05D0\u05D5\u05EA\u05D5 \u05D1\u05E8\u05E6\u05E3 \u05D0\u05D7\
  \u05E8. \u05EA\u05DB\u05E0\u05EA\u05D9 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA\
  \ \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05EA\u05E7\u05DF \u05E9\u05D2\u05D9\u05D0\
  \u05D5\u05EA, \u05DC\u05E2\u05D3\u05DB\u05DF \u05DE\u05D9\u05D3\u05E2 \u05D0\u05D5\
  \ \u05DC\u05E9\u05E0\u05D5\u05EA \u05E4\u05D5\u05E8\u05DE\u05D8."
title: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8"
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

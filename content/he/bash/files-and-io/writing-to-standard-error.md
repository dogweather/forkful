---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:49.995037-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : \u05D1-Bash, \u05D0\u05EA\u05D4 \u05DE\u05E9\u05EA\u05DE\u05E9 \u05D1-`>&2` \u05DB\
  \u05D3\u05D9 \u05DC\u05D4\u05E4\u05E0\u05D5\u05EA \u05E4\u05DC\u05D8 \u05DC-stderr.\
  \ \u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D4 \u05D1\u05E1\u05D9\u05E1\u05D9\
  \u05EA."
lastmod: '2024-03-13T22:44:39.652085-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Bash, \u05D0\u05EA\u05D4 \u05DE\u05E9\u05EA\u05DE\u05E9 \u05D1-`>&2`\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E4\u05E0\u05D5\u05EA \u05E4\u05DC\u05D8 \u05DC\
  -stderr."
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
weight: 25
---

## איך לעשות זאת:
ב-Bash, אתה משתמש ב-`>&2` כדי להפנות פלט ל-stderr. הנה דוגמה בסיסית:

```bash
echo "זו הודעה רגילה"
echo "זו הודעת שגיאה" >&2
```

הרצת סקריפט זה תציג את שתי ההודעות בקונסול, אך אם תפנה אותם, תוכל להפריד בין ה-stdout ל-stderr. לדוגמה:

```bash
bash script.sh > output.txt 2> error.txt
```

`output.txt` יכיל את `"זו הודעה רגילה"`, בעוד ש-`error.txt` יתעד `"זו הודעת שגיאה"`.

למקרה שימוש מעשי, שקול סקריפט שמעבד קבצים ומדווח על שגיאה אם קובץ לא קיים:

```bash
filename="example.txt"

if [ ! -f "$filename" ]; then
    echo "$filename לא קיים!" >&2
    exit 1
else
    echo "מעבד את $filename"
fi
```

פלט לדוגמה ישירות בקונסול כאשר `example.txt` לא קיים:

```
example.txt לא קיים!
```

אין ספריות צד שלישי ישירות ב-Bash לטיפול ב-stderr, שכן הכוונה מחודשת נתמכת כברירת מחדל ובדרך כלל מספיקה. עם זאת, ליישומים מורכבים יותר, ניתן להכניס מסגרות רישום או כלים חיצוניים לרישום כמו `syslog` או `log4bash` כדי לנהל את ה-stdout וה-stderr בצורה יעילה יותר.

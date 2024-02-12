---
title:                "כתיבה לשגיאה התקנית"
aliases:
- /he/bash/writing-to-standard-error.md
date:                  2024-02-03T19:33:49.995037-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבה לשגיאה התקנית"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לשגיאה סטנדרטית (stderr) ב-Bash היא על כיוון הודעות שגיאה או פלט אבחוני חשוב נפרד מהפלט הסטנדרטי (stdout). מתכנתים עושים זאת כדי לוודא שניתן לזהות בקלות הודעות שגיאה, לרשום אותן בלוג או אף להתעלם מהן, תוך סיוע בתהליכי דיבאג ורישום.

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

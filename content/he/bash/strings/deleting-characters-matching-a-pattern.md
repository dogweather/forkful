---
date: 2024-01-20 17:41:42.110583-07:00
description: "\u05DB\u05D0\u05E9\u05E8 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\u05D3\
  \u05D1\u05E8\u05D9\u05DD \u05E2\u05DC \u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\
  \u05D5\u05D9\u05DD \u05E9\u05DE\u05EA\u05D0\u05D9\u05DE\u05D9\u05DD \u05DC\u05D3\
  \u05E4\u05D5\u05E1 \u05DE\u05E1\u05D5\u05D9\u05DD, \u05D0\u05E0\u05D7\u05E0\u05D5\
  \ \u05D1\u05E2\u05E6\u05DD \u05DE\u05EA\u05D9\u05D9\u05D7\u05E1\u05D9\u05DD \u05DC\
  \u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E9\u05D1\
  \u05D4 \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD \u05E9\u05D0\u05E0\u05D7\u05E0\u05D5\
  \ \u05DC\u05D0 \u05E8\u05D5\u05E6\u05D9\u05DD \u05E0\u05DE\u05D7\u05E7\u05D5. \u05D6\
  \u05D4 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E0\u05E4\u05D5\u05E5 \u05D1\u05E2\u05D9\
  \u05D1\u05D5\u05D3 \u05D8\u05E7\u05E1\u05D8,\u2026"
lastmod: 2024-02-19 22:04:58.845165
model: gpt-4-1106-preview
summary: "\u05DB\u05D0\u05E9\u05E8 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\u05D3\u05D1\
  \u05E8\u05D9\u05DD \u05E2\u05DC \u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\
  \u05D9\u05DD \u05E9\u05DE\u05EA\u05D0\u05D9\u05DE\u05D9\u05DD \u05DC\u05D3\u05E4\
  \u05D5\u05E1 \u05DE\u05E1\u05D5\u05D9\u05DD, \u05D0\u05E0\u05D7\u05E0\u05D5 \u05D1\
  \u05E2\u05E6\u05DD \u05DE\u05EA\u05D9\u05D9\u05D7\u05E1\u05D9\u05DD \u05DC\u05D9\
  \u05E6\u05D9\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E9\u05D1\u05D4\
  \ \u05D4\u05EA\u05D5\u05D5\u05D9\u05DD \u05E9\u05D0\u05E0\u05D7\u05E0\u05D5 \u05DC\
  \u05D0 \u05E8\u05D5\u05E6\u05D9\u05DD \u05E0\u05DE\u05D7\u05E7\u05D5. \u05D6\u05D4\
  \ \u05EA\u05D4\u05DC\u05D9\u05DA \u05E0\u05E4\u05D5\u05E5 \u05D1\u05E2\u05D9\u05D1\
  \u05D5\u05D3 \u05D8\u05E7\u05E1\u05D8,\u2026"
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
כאשר אנחנו מדברים על מחיקת תווים שמתאימים לדפוס מסוים, אנחנו בעצם מתייחסים ליצירת מחרוזת שבה התווים שאנחנו לא רוצים נמחקו. זה תהליך נפוץ בעיבוד טקסט, ניקוי נתונים ובהכנתם לעיבוד נוסף.

## איך לעשות:
להלן דוגמה של מחיקת תווים בעזרת `tr` וביטויים רגולריים:

```Bash
# מחיקת כל האותיות הקטנות ממחרוזת
echo "Hello World" | tr -d 'a-z'
# תוצאה: H W

# מחיקת תווים שהם לא מספרים
echo "User123" | tr -cd '0-9'
# תוצאה: 123

# שימוש ב-grep למחיקת שורות המכילות דפוס מסוים
echo -e "apple\nbanana\ncherry" | grep -v 'a'
# תוצאה:
# cherry
```

## ניפוח
מחיקת תווים היא חלק אינטגרלי מן הפקת נתונים ועיבודם מתקופת ה-UNIX המוקדמת, כאשר כלים כמו `sed`, `awk`, ו`tr` היו בהתמחה רבה של מומחי IT. קיימים גם כלים חדשי יותר כמו `perl` ו`python` שמציעים טיפול יותר עשיר בביטויים רגולריים וטיפול במחרוזות. תוכניות קוד פתוח כמו `grep` מאפשרות סינון שורות לפי דפוסים, והן מיושמות תוך שימוש באלגוריתמים מפותחים להתאמת טקסט.

## ראה גם
- [GNU `grep`](https://www.gnu.org/software/grep/manual/grep.html)
- [GNU `tr` manual](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/)

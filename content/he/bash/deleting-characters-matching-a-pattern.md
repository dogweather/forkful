---
title:                "מחיקת תווים התואמים לתבנית"
date:                  2024-01-20T17:41:42.110583-07:00
model:                 gpt-4-1106-preview
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/deleting-characters-matching-a-pattern.md"
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
---
title:    "Bash: המרת מחרוזת לאותיות קטנות"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה
מדוע ייתכן שתחשוב על להמיר מחרוזת לאותיות קטנות בכתיבת הקוד שלך? אחת הסיבות העיקריות היא כי לעיתים קרובות נדרש להשוות בין שני מחרוזות בצורה אחידה, ואחת הדרכים הפשוטות ביותר לעשות זאת היא להמיר את שתי המחרוזות לאותיות קטנות.

## איך לעשות זאת
תהליך ההמרה של מחרוזת לאותיות קטנות זהה בכל הפלטפורמות בלשונת ה- bash. בשפת bash, ישנם שני פקודות ייעודיות להמרת מחרוזת לאותיות קטנות:

```bash
#הפקודה הראשונה היא להשתמש בלשונית tr עם אפשרות "--lowercase"
echo "HELLO" | tr --lowercase # פלט: hello

#הפקודה השנייה היא להשתמש בפקודת bash המובנת "LOWERCASE"
echo "HELLO" | bash -c "LOWERCASE=$1; echo $LOWERCASE" # פלט: hello
```

## שטיפה עמוקה
בלשונת ה- bash, המחרוזת מהווה משתנה משנה כדי שנוכל לפעול במקרה הזה על ידי עבודה עם גלילאווריט וספירת המחרוזת המקורית. ישנם גם אפשרויות נוספות להמרת אותיות בתוך לשונית ה- bash, כמו שימוש בפקודת פיי (tr) או פקודות נוספות עם הערות, תנאים או אפשרויות בעברית של משתנים.

## ראה גם
* [מדריך מפורט לשפת ה- bash](https://www.gnu.org/software/bash/manual/bash.html)
* [פקודת tr של GNU coreutils](https://www.gnu.org/software/coreutils/tr)
* [ויקיפדיה - שפת ה- bash](https://he.wikipedia.org/wiki/Bash)
---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##מה ולמה?

יצירת קובץ זמני היא תהליך שבו תוכנית מייצרת קובץ במערכת, בו היא יכולה לשמור מדעים מהירים. מתכנתים נוטים לעשות זאת כדי לשמור על מודעים למשתמש או לתהליכים אחרים שעלולים ללמכות אותם.

##איך לעשות:

הנה דוגמה פשוטה של כיצד ליצור קובץ זמני עם shell של Fish:

```Fish Shell
set temp_file (mktemp)
echo 'Some temporary data' > $temp_file
echo $temp_file
```

כאן, `mktemp` מייצרת קובץ זמני, ו'echo' ישמר בו את המידע הזמני.

##שיעור מעמיק:

יצירת קבצים זמניים התפתחה כדרך לשמור על מדעים למשתמש במהלך תהליכי ריצת תוכנה. ישנם אלטרנטיבות לשיטה זו, כולל שימוש בממשקי API יעודיים לניהול מדעים, אך יצירת קבצי זמני נמשכת מרוב רב סיבות, בהן פשטות ונוחות. במהלך היצירה, המשתמש והשפה שולטים בנתבים ובשמות של קבצים אלו, מה שנותן להם יתרון לעומת שיטות אחרות.

##ראו גם:

- [מדריך החלפת אינטרפרטר בספריית GNU C](https://www.gnu.org/software/libc/manual/html_node/Executing-a-File.html)
- [הסבר על מנגנון העבודה של mktemp](https://www.gnu.org/software/autogen/mktemp.html)
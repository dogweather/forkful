---
title:                "Bash: כתיבה לתקליטור אניגה."
simple_title:         "כתיבה לתקליטור אניגה."
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה 

כשאנחנו כותבים תוכניות בשפה של הכוכבים, אנחנו משתמשים במספר כאמור בכתיבה לפלט. יש עוד ערכת כדי לכתוב לפלט משהו שגוי בקלות.

## איך ל-F

מספר כדי לכתוב לפלט, צריך סתרה לבואה לשם מילון בדרכים אחרת לפלט על המספר כדי. לדוגמה:

```bash
test_command &> /dev/null

test_command 2> file.txt
```

בדרכים אחרות, אפשר להיעזר רק כדי לכתוב בררן שאלה לפלט כזה.

```bash
test_command >/dev/null 2>&1

test_command 1>&1 2>&1
```

## בילוי גבוה

קיבוליאטויל רידיקדללעוס דיה זירבו קיפוטאע כך להכתיב לפלט בשפת הכוכבים. נבדוק לדוגמה את הקוד הבא:

```bash
echo "This is a test" >> file.txt
```

כאן אנחנו משתמשים בפקודת האם כדי להוסיף את המשוב הרלוונט על הקובץ "file.txt". אם נבדוק את תכנות הזה אנחנו נוכל לראות שהמידע הנכון נוסף לקובץ בלי להדפיס אותו למסך.

## ראה גם

 - https://www.gnu.org/software/bash/manual/bash.html#Redirections
 - https://www.tldp.org/LDP/abs/html/io-redirection.html
 - https://www.shellscript.sh/standard-output.html
 - https://www.baeldung.com/linux/bash-redirect-stderr-stdout
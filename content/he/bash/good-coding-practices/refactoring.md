---
title:                "רפקטורינג"
aliases:
- /he/bash/refactoring.md
date:                  2024-01-26T01:17:43.185702-07:00
model:                 gpt-4-0125-preview
simple_title:         "רפקטורינג"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/refactoring.md"
---

{{< edit_this_page >}}

## מה ולמה?
ריפקטורינג הוא תהליך של מבנה מחדש של קוד מחשב קיים מבלי לשנות את התנהגותו החיצונית. זהו מעשה חיוני לצורך הפחתת מורכבות, שיפור הניתנות לתחזוקה, ושמירה על בריאות ונוחות ההבנה של מאגר הקוד עבור מפתחים נוכחיים ועתידיים.

## איך לעשות:
בואו נשקול סקריפט Bash פשוט שדורש ריפקטורינג. הוא מועקם, עם קוד חוזר על עצמו וקשה לעקוב אחריו:

```Bash
#!/bin/bash
echo "הזן שם קובץ:"
read filename
if [ -f "$filename" ]; then
    echo "הקובץ קיים."
    count=$(grep -c "foo" "$filename")
    echo "המילה foo מופיעה $count פעמים."
else
    echo "הקובץ לא קיים."
fi
```

לשם הבהרה ושימוש חוזר ייתכן ותהיה התערבות של הצגת פונקציות וטיפול יותר חסדי בשגיאות:

```Bash
#!/bin/bash

function file_exists() {
    [[ -f "$1" ]]
}

function count_occurrences() {
    grep -c "$1" "$2"
}

function main() {
    local filename word count
    echo "הזן שם קובץ:"
    read -r filename
    echo "הזן את המילה לחיפוש:"
    read -r word

    if file_exists "$filename"; then
        count=$(count_occurrences "$word" "$filename")
        echo "המילה $word מופיעה $count פעמים."
    else
        echo "הקובץ לא קיים." >&2
        exit 1
    fi
}

main "$@"
```

הגרסה שעברה ריפקטורינג משתמשת בפונקציות כדי לשפר את קריאות הקוד ומאפשרת שימוש חוזר אפשרי.

## חקירה עמוקה:
ריפקטורינג אינו מושג שהתחיל עם Bash או אפילו עם שפות תכנות רמה-גבוהה; הוא מושג עתיק כתכנות עצמו. המונח קיבל פורמליזציה בספר "Refactoring: Improving the Design of Existing Code" מאת מרטין פאולר ב-1999, שמתמקד בעיקר בשפות מונחות עצם.

בהקשר של כתיבת סקריפטים ב-Bash, ריפקטורינג לעיתים פירושו פיצול סקריפטים ארוכים לפונקציות, הפחתת חזרתיות באמצעות לולאות או תנאים, ומניעת שגיאות נפוצות כמו אי טיפול ברווחים בשמות קבצים. חלופות ל-Bash עבור סקריפטים שהפכו מורכבים מכדי לכלול Python או Perl, אשר מציעים מבני נתונים טובים יותר וטיפול בשגיאות עבור משימות מורכבות.

ריפקטורינג ספציפי ל-Bash מתמקד יותר בהיצמדות למעשים הטובים ביותר, כמו הציטוט של משתנים, השימוש ב-`[[ ]]` לבדיקות במקום `[ ]`, והעדפת `printf` על פני `echo` לפלט נחוש. פרטי היישום לעיתים נעים סביב הצמדה למדריכי סגנון והשימוש בכלים כמו `shellcheck` לניתוח סטטי כדי לתפוס שגיאות נפוצות.

## ראה גם:
- [מדריך סגנון של Shell מבית Google](https://google.github.io/styleguide/shellguide.html)
- [ShellCheck, כלי לניתוח סטטי של סקריפטי של](https://www.shellcheck.net/)
- [אומנות שורת הפקודה](https://github.com/jlevy/the-art-of-command-line)

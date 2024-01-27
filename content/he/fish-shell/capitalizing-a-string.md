---
title:                "הפיכת מחרוזת לאותיות רישיות"
date:                  2024-01-19
html_title:           "Bash: הפיכת מחרוזת לאותיות רישיות"
simple_title:         "הפיכת מחרוזת לאותיות רישיות"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
הפיכת מחרוזת לאותיות רישיות היא תהליך שבו משנים את האותיות הראשונות של כל מילה במחרוזת לאות גדולה. תכניתנים משתמשים בזה לשם עיצוב טקסט, הקלה על קריאה או עמידה בקונבנציות קוד.

## איך לעשות:
```Fish Shell
function capitalize
    for word in (string split " " $argv)
        set first_char (echo $word | string sub -l 1 | string upper)
        echo -n $first_char
        echo (echo $word | string sub -s 2 | string lower)
    end
end

set my_string "שלום לכולם בדוגמה זו"
capitalize $my_string
# Output:
# שלום לכולם בדוגמה זו
```

## צלילה עמוקה:
הפיכת מחרוזות לרישיות אינה חדשה והיא קיימת כבר מאז שפות התכנות הראשונות נכנסו לשימוש. בשפות אחרות יש פונקציית עזר מובנית, אבל ב-Fish Shell אנחנו יוצרים פונקציה בעצמנו. הפונקציה שראינו מפרקת כל מילה, מרימה את האות הראשונה ואז מחזירה אותה למחרוזת. אתה יכול לשחק עם שיטות אלו, כולל שימוש ב-expressions רגולריים, בהתאם לצרכים שלך.

## גם כדאי לראות:
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [String Manipulation in Fish Shell](https://fishshell.com/docs/current/commands.html#string)
- [Programming Style Guide](https://google.github.io/styleguide/shellguide.html)

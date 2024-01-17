---
title:                "קריאת ארגומנטים בשורת פקודה"
html_title:           "PowerShell: קריאת ארגומנטים בשורת פקודה"
simple_title:         "קריאת ארגומנטים בשורת פקודה"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?

קריאת ארגומנטים מפקודת השורת פקודה היא פעולה פשוטה וחיונית בתכנות פוארשל. היא מאפשרת למתכנתים לספק מידע נוסף לתוכנית שהיא מריצה, ולהתאים את הספריית פקודות והנתונים לצורכי הביצוע של כל תרחיש.

## כיצד לעשות זאת:

באמצעות פוארשל, קריאת ארגומנטים נעשית באמצעות הפרמטר " $args ", שמכיל מערך של כל הארגומנטים שהוקשו כחלק מפקודת השורת הפקודה. לדוגמה:
פקודת השורת פקודה: ```powershell
C:\Scripts\my_script.ps1 -file "C:\Users\JohnDoe\file.txt"
```

הארגומנטים המקבילים יתווספו למערך " $args ", כך שניתן יהיה להגיש עם השורת הפקודה למתכנת שביצע את הערכים הנתונים לארגומנטים הנדרשים בשם הספציפי של התוכנה. ניתן לגשת לארגומנטים על ידי מילות מפתח כגון " $args [0] ", " $args [1] ", וכן הלאה.

## לצפצף:

פקודת השורת פקודה של המשטרה Windows הייתה אחת המקורות הראשונים של קריאת ארגומנטים בפוארשל. מתכננים אחרים ביצעו גם עדכון לפוארשל כנדרש בו undefined פרימוטיבים כגון לא מקוריים, מתכננים אחרים משתמשים בתוכניות אלטרנטיביות כגון pg_switcher או Ruby, התוכנית שלל הנתונים תוכניות הרובוטיות “CONFIG.TMP” עבור זאת לחיצה על מכסי חלון כדי שתמצע (lifecycle) vu

## לראות גם:

1. [קריאת ארגומנטים בפוארשל](https://docs.microsoft.com/he-IL/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7)
2. [חשיבות של פרמטרים וריבויים בפוארשל](https://docs.microsoft.com/he-IL/powershell/scripting/dev-cross-plat/writing-cross-platform-powershell-script?view=powershell-7)
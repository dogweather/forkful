---
title:                "Bash: עבודה עם yaml"
simple_title:         "עבודה עם yaml"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה:
כתיבת פקודות בש עוזרת לנו לנתח, לעבד ולנהל קבצי נתונים מגוונים. במאמר זה נתייחס לשימוש בשפת YAML תוך שימוש בפקודות בש ליצירת, קריאה ועריכת קובץ YAML.

## איך לעבוד עם YAML בש:
כדי לעבוד עם YAML בש עלינו להתקין את חבילת הפקודות "yaml". לאחר מכן נעבוד עם הפקודות בתוך סביבת הפיתוח שלנו.

הנה דוגמה ליצירת קובץ YAML בעזרת בש:
```Bash
#!/bin/bash

# התחברות לקובץ ויצירת קובץ חדש
cat << EOF > file.yaml
name: John
age: 30
occupation: programmer
EOF
```
כאן אנחנו משתמשים בפקודה "cat" כדי ליצור ולמלא את התוכן של הקובץ בשם "file.yaml". בשורות הבאות אנו מציגים דוגמאות נוספות כיצד לקרוא ולערוך קבצי YAML בש.

```Bash
# קריאת תוכן הקובץ
cat file.yaml
# Output: name: John
#         age: 30
#         occupation: programmer

# הוספת שדה חדש לקובץ
sed -i 's/occupation.*/occupation: designer/' file.yaml

# קריאת שדה ספציפי
awk '/age/{print $2}' file.yaml
# Output: 30
```

## חפירה מעמיקה:
YAML היא שפת תסדיר נתונים פשוטה וקריאה, המתאימה בעיקר לקבצי תצורה והגדרות. בשימוש ב YAML בש, בניסוח ה- "פייתון Pythonic" עם שימוש בספריית הפקודות העשירה yaml, אנחנו יכולים לפרש קבצי YAML ולטפל בהם בקלות.

קבצי YAML נוחים לשימוש עבור המשתמשים הפעילים בש, מי שיש להם טקסט בסיסי ושיש להם צורך להעביר את הקבצים הללו בין קבצי ממשק באופן תדיר.

## ראה גם:
- תיעוד רשמי של YAML בש: https://gist.github.com/indrayam/bf92c834a0ce980166ebeb5d81bb7e7a
-
---
title:                "כתיבת בדיקות"
aliases:
- he/bash/writing-tests.md
date:                  2024-02-03T19:30:10.412941-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבת בדיקות"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות ב-Bash כוללת כתיבת סצנריות בדיקה לאימות פונקציונליות של סקריפטים שלכם ב-Bash. מתכנתים מבצעים בדיקות כדי לוודא שהסקריפטים שלהם פועלים כצפוי בתנאים שונים, תוך תיפוס שגיאות ובאגים לפני פריסה.

## איך לעשות זאת:
ב-Bash אין מסגרת בדיקות מובנית, אך אפשר לכתוב פונקציות בדיקה פשוטות. לבדיקות מתקדמות יותר, כלים צד שלישי כמו `bats-core` הם פופולריים.

### דוגמה לבדיקת בסיס ב-Bash טהור:
```bash
function test_example_function {
  result=$(your_function 'test_input')
  expected_output="expected_output"
  
  if [[ "$result" == "$expected_output" ]]; then
    echo "הבדיקה עברה."
    return 0
  else
    echo "הבדיקה נכשלה. מצופה: '$expected_output', קיבל: '$result'"
    return 1
  fi
}

# קריאה לפונקציית הבדיקה
test_example_function
```
פלט לדוגמה:
```
הבדיקה עברה.
```

### שימוש ב-`bats-core` לבדיקות:
ראשית, התקן את `bats-core`. זה לרוב נעשה דרך מנהל החבילות שלך או על ידי שיבוט של המאגר שלהם.

לאחר מכן, כתוב את בדיקותיך בקבצי `.bats` נפרדים.

```bash
# קובץ: example_function.bats

#!/usr/bin/env bats

@test "בדיקת פונקציה לדוגמה" {
  result="$(your_function 'test_input')"
  expected_output="expected_output"
  
  [ "$result" == "$expected_output" ]
}
```
כדי להריץ את הבדיקות שלך, פשוט הפעל את קובץ ה-`.bats`:
```bash
bats example_function.bats
```
פלט לדוגמה:
```
 ✓ בדיקת פונקציה לדוגמה

1 בדיקה, 0 כשלונות
```

הגישה הזו מאפשרת לך להשתלב בקלות עם בדיקות בתהליכי הפיתוח שלך, מבטיחה את אמינותם ויציבותם של הסקריפטים שלך ב-Bash.

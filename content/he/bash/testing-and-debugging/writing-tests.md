---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:10.412941-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : \u05D1-Bash \u05D0\u05D9\u05DF \u05DE\u05E1\u05D2\u05E8\u05EA \u05D1\u05D3\u05D9\
  \u05E7\u05D5\u05EA \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA, \u05D0\u05DA \u05D0\u05E4\
  \u05E9\u05E8 \u05DC\u05DB\u05EA\u05D5\u05D1 \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\
  \u05D5\u05EA \u05D1\u05D3\u05D9\u05E7\u05D4 \u05E4\u05E9\u05D5\u05D8\u05D5\u05EA\
  . \u05DC\u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05DE\u05EA\u05E7\u05D3\u05DE\u05D5\
  \u05EA \u05D9\u05D5\u05EA\u05E8, \u05DB\u05DC\u05D9\u05DD \u05E6\u05D3 \u05E9\u05DC\
  \u05D9\u05E9\u05D9 \u05DB\u05DE\u05D5 `bats-core` \u05D4\u05DD \u05E4\u05D5\u05E4\
  \u05D5\u05DC\u05E8\u05D9\u05D9\u05DD."
lastmod: '2024-03-13T22:44:39.630715-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Bash \u05D0\u05D9\u05DF \u05DE\u05E1\u05D2\u05E8\u05EA \u05D1\u05D3\
  \u05D9\u05E7\u05D5\u05EA \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA, \u05D0\u05DA \u05D0\
  \u05E4\u05E9\u05E8 \u05DC\u05DB\u05EA\u05D5\u05D1 \u05E4\u05D5\u05E0\u05E7\u05E6\
  \u05D9\u05D5\u05EA \u05D1\u05D3\u05D9\u05E7\u05D4 \u05E4\u05E9\u05D5\u05D8\u05D5\
  \u05EA."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA"
weight: 36
---

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

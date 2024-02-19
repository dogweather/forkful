---
aliases:
- /he/fish-shell/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:18.440463-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA\
  \ \u05D1-Fish Shell \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA\
  \ \u05E1\u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD \u05E9\u05DE\u05E8\u05D9\u05E6\
  \u05D9\u05DD \u05D0\u05EA \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA \u05D1\u05D0\
  \u05D5\u05E4\u05DF \u05D0\u05D5\u05D8\u05D5\u05DE\u05D8\u05D9 \u05DB\u05D3\u05D9\
  \ \u05DC\u05D0\u05DE\u05EA \u05D0\u05EA \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\u05EA\
  \u05D5 \u05DC\u05E2\u05D5\u05DE\u05EA \u05EA\u05D5\u05E6\u05D0\u05D5\u05EA \u05DE\
  \u05E6\u05D5\u05E4\u05D5\u05EA. \u05D4\u05EA\u05E8\u05D2\u05D5\u05DC \u05D4\u05D6\
  \u05D4 \u05D7\u05E9\u05D5\u05D1 \u05DE\u05E9\u05D5\u05DD \u05E9\u05D4\u05D5\u05D0\
  \ \u05DE\u05D1\u05D8\u05D9\u05D7\u2026"
lastmod: 2024-02-18 23:08:53.298953
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05D1\
  -Fish Shell \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA \u05E1\
  \u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD \u05E9\u05DE\u05E8\u05D9\u05E6\u05D9\u05DD\
  \ \u05D0\u05EA \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA \u05D1\u05D0\u05D5\u05E4\
  \u05DF \u05D0\u05D5\u05D8\u05D5\u05DE\u05D8\u05D9 \u05DB\u05D3\u05D9 \u05DC\u05D0\
  \u05DE\u05EA \u05D0\u05EA \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\u05EA\u05D5 \u05DC\
  \u05E2\u05D5\u05DE\u05EA \u05EA\u05D5\u05E6\u05D0\u05D5\u05EA \u05DE\u05E6\u05D5\
  \u05E4\u05D5\u05EA. \u05D4\u05EA\u05E8\u05D2\u05D5\u05DC \u05D4\u05D6\u05D4 \u05D7\
  \u05E9\u05D5\u05D1 \u05DE\u05E9\u05D5\u05DD \u05E9\u05D4\u05D5\u05D0 \u05DE\u05D1\
  \u05D8\u05D9\u05D7\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

כתיבת בדיקות ב-Fish Shell כוללת יצירת סקריפטים שמריצים את הקוד שלך באופן אוטומטי כדי לאמת את התנהגותו לעומת תוצאות מצופות. התרגול הזה חשוב משום שהוא מבטיח שסקריפטי ה-shell שלך פועלים כפי שתכננת, תופסים שגיאות בשלב מוקדם והופכים את התחזוקה לקלה יותר.

## איך לעשות:

ל-Fish אין מסגרת בדיקות מובנית כמו בסביבות תכנות אחרות. עם זאת, אתה יכול לכתוב סקריפטי בדיקה פשוטים שמשתמשים בהבטחות כדי לבדוק את התנהגות הפונקציות שלך. בנוסף, אתה יכול לנצל כלים צד שלישי כמו `fishtape` לערכת בדיקות יותר מקיפה.

### דוגמה 1: סקריפט בדיקה בסיסי

בואו נתחיל עם פונקציה בסיסית ב-Fish שמחשבת את הסכום של שני מספרים:

```fish
function add --description 'הוסף שני מספרים'
    set -l sum (math $argv[1] + $argv[2])
    echo $sum
end
```

אתה יכול לכתוב סקריפט בדיקה בסיסי עבור פונקציה זו כך:

```fish
function test_add
    set -l result (add 3 4)
    if test $result -eq 7
        echo "test_add עבר"
    else
        echo "test_add נכשל"
    end
end

test_add
```

הרצת סקריפט זה תפיק:

```
test_add עבר
```

### דוגמה 2: שימוש ב-Fishtape

לפתרון בדיקה חזק יותר, ניתן להשתמש ב-`fishtape`, מריץ בדיקות המפיק TAP עבור Fish.

ראשית, התקן את `fishtape` אם טרם עשית זאת:

```fish
fisher install jorgebucaran/fishtape
```

לאחר מכן, צור קובץ בדיקה עבור הפונקציה `add` שלך, למשל, `add_test.fish`:

```fish
test "הוספת 3 ו-4 מניבה 7"
    set result (add 3 4)
    echo "$result" | fishtape
end
```

כדי להריץ את הבדיקה, השתמש בפקודה הבאה:

```fish
fishtape add_test.fish
```

פלט לדוגמה עשוי להיראות כך:

```
TAP version 13
# הוספת 3 ו-4 מניבה 7
ok 1 - test_add עבר
```

זה מודיע לך שהבדיקה עברה בהצלחה. `fishtape` מאפשר לך למבנות בדיקות מפורטות יותר ומספק פלט מודיעין, מה שמקל על ניפוי באגים ומספק כיסוי בדיקות מקיף עבור סקריפטי ה-Fish שלך.

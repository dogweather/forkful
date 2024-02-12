---
title:                "כתיבת בדיקות"
aliases:
- /he/fish-shell/writing-tests/
date:                  2024-02-03T19:31:18.440463-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבת בדיקות"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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

---
date: 2024-01-26 03:49:46.140062-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : \u05DC-Fish \u05D0\u05D9\u05DF \u05DE\u05E0\u05E4\u05D4 \u05E9\u05D2\u05D9\u05D0\
  \u05D5\u05EA \u05DE\u05D5\u05D1\u05E0\u05D4 \u05DB\u05DE\u05D5 \u05E9\u05E7\u05D9\
  \u05D9\u05DD \u05D1\u05E9\u05E8\u05DC\u05D9\u05DD \u05D0\u05D7\u05E8\u05D9\u05DD\
  , \u05D0\u05DA \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\
  \u05DB\u05DC\u05D9\u05DD \u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05D9\u05DD \u05DB\
  \u05DE\u05D5 `gdb` \u05DC\u05E0\u05D9\u05E4\u05D5\u05D9 \u05E9\u05D2\u05D9\u05D0\
  \u05D5\u05EA \u05E9\u05DC \u05EA\u05DB\u05E0\u05D9\u05D5\u05EA \u05DE\u05E7\u05D5\
  \u05DE\u05E4\u05DC\u05D5\u05EA \u05D0\u05D5 `fish -d`\u2026"
lastmod: '2024-03-13T22:44:40.056672-06:00'
model: gpt-4-0125-preview
summary: "\u05DC-Fish \u05D0\u05D9\u05DF \u05DE\u05E0\u05E4\u05D4 \u05E9\u05D2\u05D9\
  \u05D0\u05D5\u05EA \u05DE\u05D5\u05D1\u05E0\u05D4 \u05DB\u05DE\u05D5 \u05E9\u05E7\
  \u05D9\u05D9\u05DD \u05D1\u05E9\u05E8\u05DC\u05D9\u05DD \u05D0\u05D7\u05E8\u05D9\
  \u05DD, \u05D0\u05DA \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9\
  \ \u05D1\u05DB\u05DC\u05D9\u05DD \u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05D9\u05DD\
  \ \u05DB\u05DE\u05D5 `gdb` \u05DC\u05E0\u05D9\u05E4\u05D5\u05D9 \u05E9\u05D2\u05D9\
  \u05D0\u05D5\u05EA \u05E9\u05DC \u05EA\u05DB\u05E0\u05D9\u05D5\u05EA \u05DE\u05E7\
  \u05D5\u05DE\u05E4\u05DC\u05D5\u05EA \u05D0\u05D5 `fish -d` \u05DC\u05D4\u05E8\u05E6\
  \u05EA fish \u05E2\u05DD \u05E4\u05DC\u05D8 \u05E0\u05D9\u05E4\u05D5\u05D9 \u05D1\
  \u05E8\u05DE\u05D5\u05EA \u05E9\u05D5\u05E0\u05D5\u05EA."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05E4\u05D4 \u05E9\u05D2\
  \u05D9\u05D0\u05D5\u05EA"
weight: 35
---

## איך לעשות זאת:
ל-Fish אין מנפה שגיאות מובנה כמו שקיים בשרלים אחרים, אך ניתן להשתמש בכלים חיצוניים כמו `gdb` לניפוי שגיאות של תכניות מקומפלות או `fish -d` להרצת fish עם פלט ניפוי ברמות שונות. בואו נתקדם עם `fish -d`:

```fish
# הרצת קליפת fish עם רמת ניפוי 2
fish -d2

# בקליפת ה-fish, בואו נבדוק פונקציה פשוטה עם באג פוטנציאלי
function test_func
    set val 42
    echo "הערך הוא $val"
    if test $val -eq 42
        echo "הכל בסדר."
    else
        echo "משהו פה לא נכון."
    end
end

# קריאה לפונקציה וצפייה בפלט הניפוי
test_func
```

הייתם רואים פלט ניפוי נוסף לפני ואחרי הרצת הפונקציה, מה שעוזר לכם לאתר בעיות.

## צלילה עמוקה
מסורתית, ניפוי שגיאות בסביבות דמויות Unix התבצע באמצעות כלים מתמחים כמו `gdb` ל-C/C++ או `pdb` ל-Python. ב-Fish, בדרך כלל תלויים בכלים חיצוניים או בתכונות מובנות כמו `functions -v` לפלט מורחב של פונקציות ו-`set -x` לעקוב אחרי שינויים במשתנים.

חלק מהאנשים בוחרים בשרלים אלטרנטיביים כמו Bash בגלל תכונות כמו `set -x` לניפוי סקריפטים. עם זאת, ל-Fish יש את הקסם שלו עם דגש על ידידותיות למשתמש ואינטראקטיביות, שיכולה להפחית את הצורך בניפוי קשה במקרים רבים.

כאשר מדובר ביישום, ניפוי סקריפט לעיתים כרוך בהרצתו עם פלט מורחב ועקבות אחרי המקום שבו משתנים מוגדרים, מבוטלים, או משתנים בדרכים לא צפויות. עם הפלט המקודד בצבעים והגישה הידידותית למשתמש של Fish, לעיתים תוכלו להימנע מהפרטים הקטנים של ניפוי – אך כשאתם תקועים, זכרו שהבהירות והמפורט הם הכלים הטובים ביותר בשליחותכם.

## ראו גם
הנה כמה עוגני תקווה בזמן שאתם טובעים בקוד:

- תיעוד Fish על ניפוי שגיאות: https://fishshell.com/docs/current/index.html#debugging
- מדריך רשמי של GDB (GNU Debugger): https://www.gnu.org/software/gdb/documentation/
- תג Stack Overflow של Fish - מקרי ניפוי שגיאות מהעולם האמיתי: https://stackoverflow.com/questions/tagged/fish
- מדריך מתקדם לכתיבת סקריפטים ב-Bash - להשוואת גישות לניפוי: https://tldp.org/LDP/abs/html/debugging.html

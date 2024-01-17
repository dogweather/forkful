---
title:                "עובדים עם yaml"
html_title:           "Haskell: עובדים עם yaml"
simple_title:         "עובדים עם yaml"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם YAML היא כלי שימושי למתכנתים המשמש לליכוד, קריאה וכתיבה של מידע בפורמט נוח ונקרא אנושית. מתכנתים משתמשים ב-YAML כדי לארגן קבצים שונים ולהעביר מידע בין מערכות שונות.

## איך לעבוד עם YAML?
```Haskell
-- בקוד זה אנו יוצרים מילון עבור נתוני YAML
המילון <- פעולות.YAML.קרא ימל.מילון "דוגמה.פקד
־"
-- ייצוא ל- YAML עם פקד הפתיחה "פקד ברירת":
['נתונים': ['מאה', 'מאה', 'שלושת']]

-- קבלת נתוני YAML מקובץ עם הפקודה כאן
קובץ <- זמינות.קובץ "דוגמה.פקד"
תוכן <- פעולות.YAML.קרד קובץ
פקד ברירת := פעולות.YAML.מאפיין תוכן
```

## צליל עמוק
YAML הוא פורמט נתונים דבר ־ מילה המופיע פעם ראשונה ב־2001 ונבו
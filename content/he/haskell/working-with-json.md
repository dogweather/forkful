---
title:                "עבודה עם json"
html_title:           "Haskell: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 
מעבדים עם JSON הוא תהליך שבו מתבצעת התנגשות בין מידע מבנת דטה ובין מידע טורפי כמו תקשורת רשת או יישום מסוים. פופולאריותו של JSON מתבססת על היכולת שלו להיות קריא וכתיבה של קוד פשוט, מה שהופך אותו לכלי חיוני למתכנתים בדרך כלל.

## איך לעשות זאת: 
נתחיל עם דוגמה פשוטה:
`Haskell
import Data.Aeson

--מיצוא JSON ממילה
encode "מרנגו"

--פלט: "\"\u05de\u05e8\u05e0\u05d2\u05d5\""
`
אתם יכולים לראות שהקלט "מרנגו" הפך למחרוזת של JSON מוצקה. 
במשך הפעלה, זהו רק דוגמה מוצקה יותר.

כעת, ננסה לטפל בתקשורת JSON יותר מתוחכמת. 
`Haskell
import Data.Aeson

--המרת מחרוזת ממילה למבנה JSON אבסטרקטי
decode "{\"key\":\"value\"}" :: Maybe Value

--פלט: Just (Object (fromList [("key",String "value")]))
`
אל תשכחו לייבא את המודול Data.Aeson בכדי להשתמש בכל האפשרויות הטובות של JSON בהרחבה הווקטורפית שלו.

## נחתור עמוק: 
אם ברצונכם לדעת על ההיסטוריה של JSON, יש להוריד את RFC 4627 שהיה הדבר שהכיר אליו את המונח JSON.
אלטרנטיבות שניתן להשתמש בהן כפתרון לJSON הן XML ו-YAML.
המימוש האקדמי לאפשרויות תכנות JSON מופיע ב-ECMA-404.

## ראו גם: 
כדי לקבל מידע נוסף על מעבדי נתונים, נוכחו וניסויים שלמותם, אפשר לסתום לדף הוויקי הרשמי של Haskell.
אם ברצונכם ללמוד יותר על Data.Aeson, ניתן להתנסות על התיעוד המלא של הספרייה הביתית.
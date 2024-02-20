---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:41.901626-07:00
description: "\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\
  \u05D8\u05D9\u05D1\u05D9\u05D9\u05DD, \u05D0\u05D5 \u05DB\u05E4\u05D9 \u05E9-Elm\
  \ \u05E7\u05D5\u05E8\u05D0\u05EA \u05DC\u05D4\u05DD, \u05DE\u05D9\u05DC\u05D5\u05E0\
  \u05D9\u05DD, \u05DE\u05DE\u05E4\u05D9\u05DD \u05DE\u05E4\u05EA\u05D7\u05D5\u05EA\
  \ \u05DC\u05E2\u05E8\u05DB\u05D9\u05DD \u05D1\u05D3\u05E8\u05DA \u05E9\u05D4\u05D5\
  \u05E4\u05DB\u05EA \u05D0\u05EA \u05D4\u05D7\u05D9\u05E4\u05D5\u05E9, \u05D4\u05D4\
  \u05DB\u05E0\u05E1\u05D4 \u05D5\u05D4\u05DE\u05D7\u05D9\u05E7\u05D4 \u05E9\u05DC\
  \ \u05E2\u05E8\u05DB\u05D9\u05DD \u05DC\u05DE\u05D4\u05D9\u05E8\u05D9\u05DD \u05D1\
  \u05DE\u05D9\u05D5\u05D7\u05D3. \u05D4\u05DD \u05D4\u05D1\u05D7\u05D9\u05E8\u05D4\
  \ \u05D4\u05E8\u05D0\u05E9\u05D5\u05E0\u05D4\u2026"
lastmod: 2024-02-19 22:04:58.413852
model: gpt-4-0125-preview
summary: "\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\
  \u05D8\u05D9\u05D1\u05D9\u05D9\u05DD, \u05D0\u05D5 \u05DB\u05E4\u05D9 \u05E9-Elm\
  \ \u05E7\u05D5\u05E8\u05D0\u05EA \u05DC\u05D4\u05DD, \u05DE\u05D9\u05DC\u05D5\u05E0\
  \u05D9\u05DD, \u05DE\u05DE\u05E4\u05D9\u05DD \u05DE\u05E4\u05EA\u05D7\u05D5\u05EA\
  \ \u05DC\u05E2\u05E8\u05DB\u05D9\u05DD \u05D1\u05D3\u05E8\u05DA \u05E9\u05D4\u05D5\
  \u05E4\u05DB\u05EA \u05D0\u05EA \u05D4\u05D7\u05D9\u05E4\u05D5\u05E9, \u05D4\u05D4\
  \u05DB\u05E0\u05E1\u05D4 \u05D5\u05D4\u05DE\u05D7\u05D9\u05E7\u05D4 \u05E9\u05DC\
  \ \u05E2\u05E8\u05DB\u05D9\u05DD \u05DC\u05DE\u05D4\u05D9\u05E8\u05D9\u05DD \u05D1\
  \u05DE\u05D9\u05D5\u05D7\u05D3. \u05D4\u05DD \u05D4\u05D1\u05D7\u05D9\u05E8\u05D4\
  \ \u05D4\u05E8\u05D0\u05E9\u05D5\u05E0\u05D4\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD\
  \ \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\u05D9\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?

מערכים אסוציאטיביים, או כפי ש-Elm קוראת להם, מילונים, ממפים מפתחות לערכים בדרך שהופכת את החיפוש, ההכנסה והמחיקה של ערכים למהירים במיוחד. הם הבחירה הראשונה שלך כשאתה צריך לעקוב אחר דברים ללא סדר מחייב, כמו העדפות משתמש או רשימות מלאי.

## איך לעשות:

ב-Elm, אתה עובד עם מילונים במודול `Dict`, אז בואו נצלול לדוגמה מהירה:

```Elm
import Dict exposing (Dict)

-- אתחול מילון עם מפתחות מחרוזת וערכי מספר שלם
exampleDict : Dict String Int
exampleDict = Dict.fromList [("apple", 5), ("banana", 2), ("orange", 8)]

-- הוספה או עדכון של ערך
updatedDict = Dict.insert "grape" 10 exampleDict

-- אחזור של ערך (שימו לב לטיפוס Maybe, כיוון שהמפתח עשוי שלא להיות נוכח)
fruitCount : Maybe Int
fruitCount = Dict.get "apple" updatedDict

-- הסרת זוג מפתח-ערך
finalDict = Dict.remove "banana" updatedDict

-- המרת מילון חזרה לרשימה
dictToList = Dict.toList finalDict
```

פלט לדוגמה כאשר מציגים את `dictToList`:

```Elm
[("apple", 5), ("grape", 10), ("orange", 8)]
```

זה מדגים את הפעולות הבסיסיות: יצירה, עדכון, גישה, ואיטרציה על מילון.

## צלילה עמוקה

מילונים ב-Elm משתמשים בפנים במבנה הידוע כעץ AVL - סוג של עץ חיפוש בינארי מאוזן עצמית. בחירה זו מאזנת בין הוודאות שפעולות כמו הכנסה, אחזור והסרה יהיו בביצועים טובים (מורכבות לוגריתמית בזמן) לבין שמירה על פשטות בעבודה עם הנתונים.

למרות היתרונות של `Dict` ב-Elm, זה לא פתרון שמתאים לכל מצב. עבור אוספים שמסודרים או צריכים להיות רצים באופן סדרתי, רשימה או מערך עשויים להתאים יותר. בנוסף, כאשר עובדים עם קבוצת מפתחות ידועה וקבועה, שימוש בסוגים מותאמים אישית (הגרסה של Elm ל-enums) יכולה להציע יותר בטיחות סוגים וכוונה ברורה יותר בקוד שלך.

באקוסיסטם של Elm, `Dict` מציעה דרך אמינה לנהל אוספים של זוגות מפתח-ערך שבהם המפתחות הם ייחודיים והסדר לא משנה. למרות שמבנים חדשים או מתוחכמים יותר עשויים להופיע, מודול ה-`Dict` נשאר כלי יסודי בארגז הכלים של מתכנת ה-Elm בזכות פשטותו ויעילותו בטיפול במערכים אסוציאטיביים.

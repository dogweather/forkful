---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:54.424833-07:00
description: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\
  \u05E0\u05D5\u05DB\u05D7\u05D9 \u05D1-TypeScript, \u05E9\u05E4\u05D4 \u05D4\u05E0\
  \u05D1\u05E0\u05D9\u05EA \u05E2\u05DC JavaScript, \u05DE\u05D0\u05E4\u05E9\u05E8\
  \u05EA \u05DC\u05DA \u05DC\u05D2\u05E9\u05EA \u05D5\u05DC\u05E2\u05E9\u05D5\u05EA\
  \ \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05D9\u05D3\u05E2 \u05D4\u05E0\u05D5\
  \u05DB\u05D7\u05D9 \u05E9\u05DC \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05D4\
  \u05E9\u05E2\u05D4. \u05DC\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\u05E2\u05D9\
  \u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05D9\u05E9 \u05E6\u05D5\
  \u05E8\u05DA\u2026"
lastmod: '2024-03-13T22:44:38.934930-06:00'
model: gpt-4-0125-preview
summary: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9 \u05D1-TypeScript, \u05E9\u05E4\u05D4 \u05D4\u05E0\u05D1\
  \u05E0\u05D9\u05EA \u05E2\u05DC JavaScript, \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA\
  \ \u05DC\u05DA \u05DC\u05D2\u05E9\u05EA \u05D5\u05DC\u05E2\u05E9\u05D5\u05EA \u05E9\
  \u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05D9\u05D3\u05E2 \u05D4\u05E0\u05D5\u05DB\
  \u05D7\u05D9 \u05E9\u05DC \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05D4\u05E9\
  \u05E2\u05D4. \u05DC\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\u05E2\u05D9\u05EA\
  \u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05D9\u05E9 \u05E6\u05D5\u05E8\
  \u05DA\u2026"
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
---

{{< edit_this_page >}}

## מה ולמה?
קבלת התאריך הנוכחי ב-TypeScript, שפה הנבנית על JavaScript, מאפשרת לך לגשת ולעשות שימוש במידע הנוכחי של התאריך והשעה. לתכנתים לעיתים קרובות יש צורך בפונקציונליות זו עבור יצירת חותמות זמן, תזמון, ותכונות אחרות הרגישות לזמן ביישומיהם.

## איך לעשות זאת:
ב-TypeScript, ניתן להשתמש באובייקט `Date` כדי לקבל את התאריך והשעה הנוכחיים. הנה כיצד תוכלו לעשות זאת:

```typescript
const currentDate = new Date();
console.log(currentDate);
```

פלט לדוגמא:
```
2023-04-12T07:20:50.52Z
```

חתיכת הקוד הזו יוצרת אובייקט `Date` חדש המכיל את התאריך והשעה הנוכחיים, אשר לאחר מכן מודפסים לקונסול. ניתן גם לעצב את התאריך באמצעות toLocaleDateString() לתבניות קריאות יותר:

```typescript
const currentDate = new Date();
console.log(currentDate.toLocaleDateString());
```

פלט לדוגמא:
```
12/4/2023
```

### שימוש ב-date-fns
למניפולציה ועיצוב תאריכים מרחיקים לכת, הספרייה `date-fns` היא בחירה פופולרית. ראשית, התקנו אותה דרך npm:

```bash
npm install date-fns
```

לאחר מכן, תוכלו להשתמש בה כדי לעצב את התאריך הנוכחי:

```typescript
import { format } from 'date-fns';

const currentDate = new Date();
console.log(format(currentDate, 'yyyy-MM-dd'));
```

פלט לדוגמא:
```
2023-04-12
```

דוגמא זו של `date-fns` מעצבת את התאריך הנוכחי כמחרוזת בתבנית "YYYY-MM-DD". הספרייה מציעה מגוון עצום של פונקציות למניפולציה של תאריכים, מה שהופך אותה לכלי גמיש עבור כל תכנת TypeScript העובד עם תאריכים.

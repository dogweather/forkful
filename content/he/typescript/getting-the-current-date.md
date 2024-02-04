---
title:                "קבלת התאריך הנוכחי"
date:                  2024-02-03T19:11:54.424833-07:00
model:                 gpt-4-0125-preview
simple_title:         "קבלת התאריך הנוכחי"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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

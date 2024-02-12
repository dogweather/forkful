---
title:                "כתיבת בדיקות"
aliases:
- /he/typescript/writing-tests/
date:                  2024-02-03T19:32:51.653439-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבת בדיקות"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות ב- TypeScript כוללת יצירת סקריפטים אוטומטיים לאימות פונקציונליות ונכונות הקוד שלך. מתכנתים עושים זאת כדי להבטיח אמינות, לתפוס באגים במהירות ולקדם גידול קוד נתון לתחזוקה, מאחר וההקלדה הסטטית של TypeScript מוסיפה רמה של ניבוי לבדיקות JavaScript.

## איך לעשות:
TypeScript פועלת בהרמוניה עם רוב מסגרות הבדיקה של JavaScript. לצורך הדגמה, נשתמש ב-Jest, מסגרת בדיקה פופולרית, בשל הטמעתה ללא תצורה לפרויקטים של TypeScript.

ראשית, ודאו ש-Jest והסוגים הדרושים של TypeScript מותקנים:

```bash
npm install --save-dev jest typescript ts-jest @types/jest
```

לאחר מכן, הגדרו את Jest לעבוד עם TypeScript על ידי שינוי ה- `jest.config.js` או ביצירת קובץ חדש:

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

כעת, בואו נכתוב פונקציה פשוטה ובדיקה עבורה. שקלו קובץ `sum.ts` עם הפונקציה הבאה:

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}
```

צרו קובץ בדיקה בשם `sum.test.ts`:

```typescript
// sum.test.ts
import { sum } from './sum';

test('מוסיף 1 + 2 לקבל 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

הפעילו את הבדיקות שלכם עם:

```bash
npx jest
```

פלט לדוגמה המציין עובר בדיקה אמור להראות משהו כזה:

```plaintext
 PASS  ./sum.test.ts
  ✓ מוסיף 1 + 2 לקבל 3 (2 ms)
```

עבור קוד אסינכרוני, Jest מתאימה עם `async/await`. נניח שיש לכם פונקציה אסינכרונית `fetchData`:

```typescript
// asyncFunctions.ts
export async function fetchData(): Promise<string> {
  return "data";
}
```

הבדיקה שלכם שמשתמשת בפונקציות אסינכרוניות:

```typescript
// asyncFunctions.test.ts
import { fetchData } from './asyncFunctions';

test('משיג נתונים בהצלחה', async () => {
  expect(await fetchData()).toBe('data');
});
```

כאשר אתם מריצים את הבדיקות שלכם, Jest תחכה שההבטחה תתממש, ותבצע בדיקה נכונה של פעולות אסינכרוניות.

זכרו, בדיקה אפקטיבית כוללת כתיבת מספר בדיקות לסצנריות שונות, כולל מקרי קצה, כדי לוודא שהקוד שלכם ב- TypeScript מתנהג כצפוי.

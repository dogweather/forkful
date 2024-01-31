---
title:                "כתיבת בדיקות"
date:                  2024-01-19
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות זה לבחון את הקוד שלנו באופן אוטומטי. אנחנו עושים את זה כדי לוודא שהתוכנה שלנו עובדת כמצופה ולזהות תקלות מוקדם.

## איך לעשות:
```TypeScript
import { expect, test } from '@jest/globals';

test('חיבור שני מספרים', () => {
  expect(1 + 2).toBe(3);
});

test('מערך מכיל איבר', () => {
  expect(['תפוח', 'בננה']).toContain('בננה');
});
```
פלט לדוגמא:
```
PASS  ./math.test.ts
✓ חיבור שני מספרים (3ms)
✓ מערך מכיל איבר
```

## עיון נוסף
בתחילה, בדיקות כתבו בצורה ידנית והשקיעו זמן רב. כיום יש ספריות כמו Jest, Mocha, ו Jasmine שמקלות על התהליך. הן מציעות דרכים שונות להגדיר בדיקות, לבדוק הנחות ולרכז את התוצאות.

## ראו גם
- [התיעוד הרשמי של Jest](https://jestjs.io/docs/getting-started)

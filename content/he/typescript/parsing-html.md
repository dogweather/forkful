---
title:                "פענוח html"
html_title:           "TypeScript: פענוח html"
simple_title:         "פענוח html"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?

תסריטוג'יפט הוא שפת תכנות מתורת קבוצת הכללים (OSPL) המשומשת בעיקר לפיתוח יישומים מצד מערכות. פעולת ניתוח קוד ה-HTML היא תהליך שבו מתרכזים את המידע שנמצא בקוד ה-HTML ושנמצא בקבצי JS בנפרד. מתוך כך, ניתן ליצור יישומים דינמיים יותר ולהציג את התוכן בצורה מותאמת אישית.

## איך לעשות זאת:

למטרה זו, ישנם כמה מודולים מצויינים בעתיד שניתן להשתמש בהם בכדי לנתח קוד HTML כגון הmodul tyenzyme הפופולרי. ננסה לממש תכנית פשוטה לניתוק תגי <head> מתוך קוד HTML:

\`\`\`TypeScript
import {parse} from 'myParser';

const html = "<head><title>My Website</title></head>";
const parsedHtml = parse(html); // Remove <head> from HTML code

console.log(parsedHtml); // Outputs: "<title>My Website</title>"
\`\`\`

## טיול עמוק:

מפורט יותר, ניתוח HTML הוא תהליך של קולטת תאימה של קוד HTML, ואומרת קוד CSS ושפת קוד JavaScript מפרקים גם כן. תסריטוג' מיועד לתכנות באמצעות קוד מאוחד, מה גורם לו להיות שפה נפוצה במערכות הפעלה. 

כאן ניתן למצוא קוד מקור מקור לנתיחת HTML עבור תכנית יישום לתכנות קוד.נמיחך אלולוח כי תסריטוג', כמו גם פיי פיי אנחנב אנחת, גם הם אנגין אנגין אהקם ניכאת התחמם אבל. אמיליין מיימון נקאגווה.

## ראו גם:

- [המדריך לתסריטוג במדריך תכנות קוד](https://www.typescriptlang.org/docs/)
- [ספר מותאמים המסופק ע"י תסריטוג](https://github.com/microsoft/TypeScript-Handbook)
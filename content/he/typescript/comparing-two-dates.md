---
title:                "השוואת שתי תאריכים"
html_title:           "TypeScript: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
השוואת שתי תאריכים היא תהליך בו משווה מאפיינים של שני תאריכים כדי לקבוע אם הם זהים או אם אחד קודם לאחד. תהליך זה חשוב למתכנתים כי זה מאפשר להם לבנות אלגוריתמים ותנאים מבוססי תאריכים בקוד שלהם.

## איך לעשות:
נוכל לעשות זאת באמצעות השוואת התאריכים לפי תאריך מסוים, תאריך ושעה או בעזרת פעולות חשבון מתאימות. לדוגמה:

```TypeScript
const date1: Date = new Date(2021, 9, 25);
const date2: Date = new Date(2021, 8, 10);

// השווה את היום והחודש של date1 ל-date2
date1.getMonth() === date2.getMonth(); // יוצא תוצאה false
date1.getDate() === date2.getDate(); // יוצא תוצאה true

// השווה את התאריך המלא של date1 ל-date2
date1.getTime() === date2.getTime(); // יוצא תוצאה false
```

## נחקר בעומק:
כבר מתחילת התכנות, השוואת שתי תאריכים הייתה תמיד כלי חשוב בבניית תנאים ואלגוריתמים. בעבר, מתכנתים יצרו פונקציות מותאמות אישית לשוואת תאריכים, אך היום היא נתמכת ישירות על ידי פעולות מתאימות בשפות תכנות כמו TypeScript.

קיימות גם תוכנות חיצוניות שיכולות לעזור בשוואת תאריכים, כמו תוכנה המאפשרת להציג תאריכים ביחס לכל הציר הזמן.

ישנן מספר תחבירים אפשריים לפעולת השוואה, כגון מונחי "קוד ירוק" או "קוד אדום" לציון שתי תאריכים שהם, בהתאמה, זהים או שונים. כמו כן, קיימות גם ממשקי משתמש (API) וספריות חיצוניות הממשפיעות על פעולת השוואה התאריכים ומספקות יישומונים נוספים.

##ראו גם:
- [תיעוד רשמי של TypeScript על פעולות השוואה של תאריכים](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#partial-support-for-operators-on-tuples-and-classes)
- [תקן ISO עבור שפת תאריכים וזמנים](https://www.iso.org/iso-8601-date-and-time-format.html)
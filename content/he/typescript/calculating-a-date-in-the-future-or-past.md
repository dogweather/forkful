---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "TypeScript: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

📝תכתובת תכנות TypeScript לקוראים ישראלים בסגנון לא פורמלי וקצר ולעניין. נשתדל לא להוסיף מילים או משפטים מיותרים. המאמר מכיל ארבעה סעיפים המופרדים בכותרות מתורגמות לעברית. ללא סעיף "מסקנה".

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא הפעולה של חישוב תאריך חדש שמבוסס על תאריך קיים וכמות ימים שמוספרת אליו. כמו כן, חישוב תאריך בעתיד או בעבר יכול לעזור למתכנתים ליצור יישומים ותכנים שיכולים להתאים לזמנים שונים, לדוגמה, לתכנן אירועים או הרשאות של משתמשים מסוימים.

## איך לעשות?
לדוגמה כדי לחשב תאריך של 7 ימים לפני התאריך הנוכחי, נצטרך להשתמש בחיסור את מספר הימים מהתאריך הנוכחי עם פונקציית התאריך המובנית של TypeScript ולהכניס את התוצאה לתוך משתנה חדש. לדוגמה:

```typescript
let today: Date = new Date();
let result: Date = new Date(today.setDate(today.getDate() - 7));
console.log(result);
```

פלט המסך:

```
2021-07-07T17:57:37.471Z
```

לחישוב תאריך בעתיד, נוסיף את מספר הימים הרצוי לתאריך הנוכחי באמצעות פונקציית התאריך המובנית ונכניס את התוצאה לתוך משתנה חדש. לדוגמה:

```typescript
let today: Date = new Date();
let result: Date = new Date(today.setDate(today.getDate() + 7));
console.log(result);
```

פלט המסך:

```
2021-07-21T17:57:37.471Z
```

## לחקור עמוק יותר
• ההיסטוריה של חישוב תאריכים בעתיד או בעבר מחדשת לאלפי שנים.
• פתרונות אלטרנטיביים לחישוב תאריכים, כגון שימוש במחלקות מתקדמות של תאריכים.
• פרטים מעניינים על פונקציות התאריך המובנות של TypeScript ואיך להשתמש בהן לחישוב תאריכים בעתיד או בעבר.

## לראות כמו כן
• [תיעוד רשמי של פונקציות התאריך המובנות של TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html#the-ecmascript-standard-library)
• [אפליקציה לחישוב תאריכים בעתיד או בעבר עם TypeScript](https://github.com/apawn/calc-date-ts)
• [פוסט על חישוב זמן מראש בעזרת TypeScript ופונקציות התאריך המובנות](https://javascriptio.com/view/114909)
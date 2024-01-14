---
title:    "TypeScript: קריאת ארגומנטים בשורת הפקודה"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

# מדוע

קריאת פרמטרי שורת הפקודה בתוכנות TypeScript היא כלי חשוב ושימושי לכל מתכנת. בכתיבת קוד, עלינו להיות יכולים לטפל במגוון נתונים ומידע, וקריאת פרמטרי שורת הפקודה מספקת לנו זאת בצורה קלה ויעילה. הבלוג הזה ילמד אותך כיצד לקרוא פרמטרי שורת הפקודה ב TypeScript ולהשתמש בזה לתת את הביצוע הטוב ביותר לתוכניות שלך.

# איך לעשות זאת

קריאת פרמטרי שורת הפקודה ב TypeScript נעשית באמצעות ספריית `process`. נדרש להתחיל מייבוא שלה באמצעות הפקודה `import` ולאחר מכן אנו יכולים להשתמש בשיטות של הספרייה כדי לקבל את הפרמטרים שהועברו לתוכנית במהלך פעולת ההפעלה. הנה מספר דוגמאות שבהן אנו מקבלים פרמטרי שורת הפקודה ומדפיסים את התוצאה המבוקשת:

```TypeScript
// קורא פרמטר יחיד:
const param = process.argv[2];
console.log(param);

// קורא פרמטר מספרי:
const params = process.argv.slice(2);
console.log(params);

// קורא פרמטרים עם שם מפתח:
const paramsObject = {};
process.argv.forEach((item, index) => {
    if (item.startsWith('--')) {
        paramsObject[item.slice(2)] = process.argv[index + 1];
    }
});
console.log(paramsObject);
```

הנה דוגמא להרצה של קוד הפקודה `ts-node app.ts hello world --user Bob` ופלט התוצאה שלה:

```bash
hello
[ 'hello', 'world', '--user', 'Bob' ]
{ user: 'Bob' }
```

# עיון מעמיק

בנוסף לפרמטרי שורת הפקודה הרגילים, ניתן גם לקרוא פרמטרים נוספים כגון כתובות אינטרנט ופעולות שונות עם המבנה `--` מובלד וערך אחריו. לדוגמא, ואנו מריצים את התוכנית הבאה `ts-node app.ts --url https://example.com --method POST --data '{"id":
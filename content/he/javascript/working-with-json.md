---
title:                "Javascript: עבודה עם json."
simple_title:         "עבודה עם json."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## מדוע

פורמט JSON הוא דרך נוחה ומוכחת של אוגד מידע במבנה שבו נוכל לקרוא וליצור את המידע. בתחום פיתוח האתרים והיישומים, JSON משמש כדרך מוכחת ופופולרית במיוחד להעברת מידע בין שרת ולקוח.

## איך לעבוד עם JSON

### יצירת אובייקט JSON

כדי ליצור אובייקט JSON נוכיח את הבנה הבסיסית שלו בעזרת קוד ותנאים בפונקציית `if`:

```javascript
// יצירת אובייקט JSON ריק
let myObj = {};

// הוספת מפתח וערך לאובייקט
myObj["key"] = "value";

// תנאי שבודק אם יש מפתח באובייקט ומעדכן את הערך אם כן
if (myObj["key"] !== undefined) {
    myObj["key"] = "new value";
}

// הדפסת האובייקט לבדיקה
console.log(myObj);

// Output: { "key": "new value" }
```

### קריאת מידע מקובץ JSON

כדי לקרוא מידע מקובץ JSON נשתמש בפונקציית `fetch` ונשתמש בפונקציות של בדיקת תנאים והמרת נתונים:

```javascript
// פונקציית fetch שמקבלת את הקובץ JSON ומחזירה Promise
fetch("data.json")

// Promise עם נתוני הקובץ הממתינים להתממשות
.then((response) => response.json())

// Promise עם הנתונים המבוקשים
.then((data) => {

    // הדפסת הנתונים למסך
    console.log(data);

    // הוצאת המידע מהנתונים וכניסתו למשתנה חדש
    let name = data.name;

    // בדיקת תנאי על המשתנה והדפסת הודעה עם המידע מהקובץ
    if (name !== undefined) {
        console.log(`My name is ${name}.`);
    }
});

// Output: My name is John Doe.
```

## העמקה

פורמט JSON מספק דרך נוחה ומותאמת לתכנות לארגן ולהעביר מידע בין-לקוחי. הוא מכיל מבנה פשוט ומובן ומכיל עיקרון חשיבה דומה למבנה תכולת האתר. בנוס
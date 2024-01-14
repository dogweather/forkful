---
title:    "TypeScript: יצירת קובץ זמני"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## למה

יצירת קובץ זמני היא תיק היכן שצריך לעבוד עם קבצים זמניים בתוך קוד TypeScript. זה מאפשר לנו לייצר קבצי זמני שימושיים עבור יישומי התקנה ותמיכה.

## איך לעשות זאת

בתחילה, נצטרך לייבא את המודול "fs" של Node.js כדי להשתמש בפונקציות המקומיות שלו ליצירת קבצים זמניים. לאחר מכן, נשתמש בפונקציית "tmpfileSync" כדי ליצור קובץ זמני.

```TypeScript
import * as fs from 'fs';

const tempFile = fs.tmpfileSync();
console.log(tempFile.name); // output: random_file_name.tmp
```

נעבור עכשיו ליצירת קובץ זמני עם תוכן מסוים. יש לנו כמה אפשרויות להתייחס לתוכן של הקובץ, אם נרצה ליצור קובץ עם תוכן ריק נוכל להשתמש בפונקציית "openSync" ולפסול את הפרמטר של תוכן הקובץ. אם רוצים ליצור קובץ עם תוכן, נוכל להשתמש בפונקציית "writeFileSync" כדי לכתוב לתוכן הקובץ.

```TypeScript
import * as fs from 'fs';

const tempFile = fs.tmpfileSync();
fs.writeFileSync(tempFile.name, 'This is a temporary file.'); // write content to temp file
```

כאשר אנחנו מסתיימים עם הקובץ הזמני, ניתן למחוק אותו באמצעות פונקציית "unlinkSync".

```TypeScript
import * as fs from 'fs';

const tempFile = fs.tmpfileSync();
fs.unlinkSync(tempFile.name); // delete temp file
```

## צליל עמוק

יצירת קובץ זמני היא דרך נהדרת לשימוש בתוך קוד TypeScript. תוכלו להשתמש בקבצים זמניים עבור מטלות כמו העלאת תמונות לכתבת יישומים ועוד. חשוב לזכור שהקבצים הזמניים ימחקו תוך זמן קצר לאחר שהם נוצרו, לכן מומלץ לשים לב ולמחוק אותם כשם שסיימתם להשתמש בה
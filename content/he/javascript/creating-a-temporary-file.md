---
title:                "יצירת קובץ זמני"
html_title:           "Javascript: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני היא פעולה שמאפשרת למתכנתים ליצור קובץ שמשמש רק לצורך זמני ואינו נשמר בצורה קבועה. פעולה זו נחשבת חשובה בהינתן שהיא מאפשרת למתכנתים לבצע פעולות שונות בצורה נקייה ואינה משאבת מקומות תוכנה על המערכת.

## כיצד ל:
```javascript
// Create a temporary file using the fs module
const fs = require('fs');

// Use the fs.mkdtemp() method to generate a temporary directory
fs.mkdtemp('/temp/', (err, folder) => {
    if (err) throw err;

    // Create a new file inside the temporary directory
    fs.writeFile(`${folder}/tempFile.txt`, 'This is a temporary file', (err) => {
        if (err) throw err;
        console.log('Temporary file created!');
    });
});

```

## העמקה:
פעולות כגון יצירת קבצים זמניים נעשו נפוצות יותר כשהיה צורך לבצע פעולות בין כמה פלטפורמות ומערכות הפעלה. פעולה זו נותנת למתכנתים את היכולת לחלץ מידע מקובץ זמני ולהתאים אותו לגדרים נכונים בפלטפורמת המערכת הנבחרת. פתרונות אלטרנטיביים כוללים ביצוע פעולות בזמן אמת על קבצים קיימים או שימוש בספריות ניהול קבצים זמניים.

## לצפות ב:
- https://nodejs.org/api/fs.html#fs_fs_mkstemp_prefix_options_callback הספריה הרשמית של Node.js על פעולות קבצים זמניים.
- https://www.codementor.io/@danielchinedu/temporary-files-in-node-js-9guobop93 עצות והנחיות ליצירת קבצים זמניים ב-Node.js
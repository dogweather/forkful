---
title:                "טיפול בשגיאות"
date:                  2024-01-26T00:53:11.188066-07:00
model:                 gpt-4-1106-preview
simple_title:         "טיפול בשגיאות"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/handling-errors.md"
---

{{< edit_this_page >}}

## מה ולמה?
טיפול בשגיאות הוא על כך להתחשב באפשרויות בהן דברים עשויים ללכת לא כשורה בקוד שלך ולנהל את המצבים הללו בחן. מתכנתים עושים זאת כי זה מבטיח שהאפליקציות שלהם יהיו עמידות ונוחות למשתמש, אפילו בעת מפגש עם הלא צפוי.

## איך לעשות:
ב-Gleam, תפעיל לעיתים קרובות את הטיפוס `Result` לניהול שגיאות. זה enum עם שתי וריאנטות: `Ok` (להצלחה) ו-`Error` (לכשלון). הנה דוגמה פשוטה:

```Gleam
pub fn might_fail(break_it: Bool) -> Result(Int, String) {
  if break_it {
    Error("אופס! זה נשבר.".to_string())
  } else {
    Ok(42)
  }
}

pub fn main() {
  let result = might_fail(False)
  case result {
    Ok(value) => value
    Error(message) => {
      io.println(message)
      0
    } 
  }
}
```

אם תריץ את `main` עם `might_fail(False)`, זה יחזיר `42`. אם תעביר `True`, זה ידפיס "אופס! זה נשבר." ויחזיר `0`.

## צלילה עמוקה
הגישה של Gleam לטיפול בשגיאות מושפעת מהשורשים שלה ב-Erlang. מסורתית, Erlang משתמשת בפילוסופיית "תן לזה להתרסק", ומסתמכת על עצי פיקוח לניהול כשלונות תהליכים. עם זאת, כאשר אתה כותב קוד Gleam שלא נמצא בתוך תהליך שאמור להיות מנוטר, כמו בתוך פונקציית ספרייה, היית רוצה לטפל בשגיאות באופן מפורש.

אלטרנטיבות לשימוש ב-`Result` כוללות את שימוש בטיפוס `Option` למקרים בהם משהו עשוי להיות `None` (כלום) או `Some` (משהו), אבל אלו לא נושאים מידע על שגיאות. להעברת סימולטורים דרך גבולות תהליך, עשוי להיות שתשתמש במנגנוני ההודעות של Erlang.

טיפול בשגיאות ב-Gleam משקף סגנון תכנות פונקציונאלי, שבו תופעות לוואי (כמו שגיאות) מנוהלות באמצעות טיפוסים והתאמת דפוסים, תוך כדי ספקת בהירות וניבוי בניהול השגיאות.

## ראה גם
- [ניהול שגיאות ב-Erlang](http://erlang.org/doc/reference_manual/errors.html)
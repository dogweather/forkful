---
title:                "Rust: כתיבה לצלצול נספח"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##למה

על מנת לפרסם מידע כתוב ולהציג מידע מפורט על תקלות ושגיאות בקוד, מתכנתים השתמשו בתכונה הנקראת "כתיבה למסך שגיאות" או כתיבה ל standard error בכדי לעזור באיתור ותיקון שגיאות בתוכניות. כתיבה ל-mustandest# הוא צורת התקשרות עם תכונה זו המאפשרת קריאת שגיאות ותיקון תקלות באופן יעיל ומוקדם בתהליך הפיתוח של תוכניות.

##כיצד

תחילה, ניצור קובץ חדש בשפת Rust ונאתחל את הספריות הנדרשות. לדוגמה:

```Rust
fn main() {
  // use the standard library to access standard error
  use std::io::Write;

  // create a new file for writing to standard error
  let mut stderr = std::io::stderr();

  // write an error message to standard error
  writeln!(stderr, "This is an error message").expect("Failed to write to standard error");
}
```

בקטע הקוד לעיל, אנחנו משתמשים בספריית התקנה של Rust ובפונקציות שלה כדי לכתוב לכתובת standard error. ניצור קובץ חדש ונכתוב אליו הודעה שגיאה כדי לבדוק את הפעולה בפועל.

הפעולה תתגייס באופן אוטומטי לתוך הסוגריים הבדוקים כאשר היא תסיים את ניסיונות התקשורת. בתהליך הזה, כתיבה ל-standard error חשובה במיוחד בכדי לאתר ולתקן שגיאות קוד בתוכניות שלנו.

##עטיפת קודים

אין רחוב משחקים או פתרונות פשוטים כאשר מדובר בתקשורת עם standard error. בכדי להבין את כל האופציות והפונקציות השונות שניתן להשתמש בהן בתהליך זה, ייתכן שתרצו להשתמש באתר המקורי של בשפת Rust או בתיעוד רשמי של הספריית standard error.

###אבני דרך

לפעמים, בעת עבודה עם כתיבה ל-standard error, ית
---
title:                "כתיבה אל תקן השגיאה"
html_title:           "PHP: כתיבה אל תקן השגיאה"
simple_title:         "כתיבה אל תקן השגיאה"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה:

כתיבה לסטנדרט שגיאה היא כלי חשוב בתוך התכנות המבוסס על PHP. היא מאפשרת למפתחים ומתכנתים לזהות ולטפל בשגיאות בקוד בצורה מהירה ויעילה. מאפשרת גם לנו להתייחס לשגיאות בתכנים שרצים בסביבת הפיתוח מוקדמת.

## איך לעשות זאת:

לכתוב לסטנדרט שגיאה בפHP פשוט מאוד. כל שנדרש הוא להשתמש בפונקציה המובנית "error_log()" ולספק לה את ההודעה שברצונך להדפיס. ניתן להשתמש במספר פרמטרים נוספים כדי להגדיר מפורט יותר את השגיאה ולשלוח את ההודעה למקומות שונים, כגון קובץ יומן או דוא"ל.

```PHP
// הדפסת שגיאה כללית
error_log("אירעה שגיאה בקוד הזה");

// הדפסת שגיאה בקובץ יומן עם פרטים מפורטים יותר
$file = "logs/errors.log";
$errorMessage = "אירעה שגיאה בקוד הזה";
$additionalInfo = ["user" => "John Doe", "page" => "index"];
error_log($errorMessage . " " . json_encode($additionalInfo), 3, $file);

// שליחת הודעת שגיאה לדוא"ל של המפתח
$to = "developer@example.com";
$subject = "שגיאה באתר";
$message = "אירעה שגיאה בקוד הזה. הסיבה: אינו מחובר למסד הנתונים";
$headers = "From: webmaster@example.com";
error_log($message, 1, $to, $subject, $headers);
```

הפונקציה "error_log()" גם מאפשרת לנו להגדיר רמת חשיבות לתצורת השגיאות. לדוגמה, ניתן לשלוח שגיאות רק לדוא"ל במצב פיתוח ולא לכל פעם שמשתמש מתקשר לאתר. ניתן גם לשלוח הודעות בכל פעם שאירעה שגיאה חמורה בשרת.

## חפשים עמוקים:

בנוסף להצגת שגיאות בזמן הפיתוח
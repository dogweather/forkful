---
title:                "שליחת בקשת http"
html_title:           "Kotlin: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

מה ולמה?

שלושיק שניות - שליחת בקשת HTTP פשוטה היא פעולה נפוצה בתכנות, שמאפשרת לקבל תוכן מכתובת אינטרנט ספציפית. זה נעשה בסיסמה בעזרת הפונקציה "גט" בכל שפת תכנות או בספרייה לכך. סיבת כמעט כל תכנית לכל מטרה היא זה לקבל תנאי או תוכן חדש מהשרת.

-

איך לעשות?

כדי לשלוח בקשת HTTP בשפת Kotlin, יש להשתמש בפונקציה "withUrl" מספריית "kotlin.http". למשל:

```Kotlin
val url = "https://example.com"
val response = withUrl(url).getAsUtf8()
```

בקוד מעל, אנחנו מציינים כתובת אינטרנט ושולחים בקשת "GET" באמצעות הפונקציה "getAsUtf8", שתוכנתה לקבל תוכן בפורמט UTF-8.

-

תמצא נמצאים

הפונקציה "withUrl" אכן חלק בלתי נפרד משפת Kotlin, אך היא לא היחידה. מתודות כמו "withRequestProperty" מספקות טיפול חכם יותר בבקשות HTTP, וגם בשפת Java ישנן ביבליות מכובדות כמו Apache HttpComponents שמאפשרות שליחת בקשות HTTP. כמו כן, ניתן להשתמש בפונקציות גם בפורמט JSON ולשלוח נתונים לשרת באמצעות תגיות.

-

רקע היסטורי, פיתוח ופרטים ממש האם

שליחת בקשות HTTP היא תהליך יסודי בתכנות אתרים ויישומים אינטרנטיים, כך שאם אתה מתכנת אתרים בקשר ל HTML וביטול CSS כפי שאתה מתמגש עם שפות תכנות אחרות, הסתכל עליהם בתקרת כדור הארץ ותקף נשיכה HTTP כמערכת "מוכנת לשמשכם", הלי את הכלים שיותר מקרבים אותו כמפתח לב "בעיצוב".

-

ראו אילו

כאן ישנם לינקים לפורומים ואתרים ידועים ב-HTTP, שימושיים לבוא בקשר לדיוני בחומר הכי רלוונטי בנוגע לשליחת בקשות HTTP:

- [The Definitive Guide to HTTP](http://www.httpguide.org)
- [Mozilla Developer Network's HTTP Documentation](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- [Stack Overflow HTTP questions](https://stackoverflow.com/questions/tagged/http)
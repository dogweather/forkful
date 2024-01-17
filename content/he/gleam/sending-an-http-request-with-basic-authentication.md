---
title:                "שליחת בקשת http עם אימות בסיסי בתכנות מחשבים"
html_title:           "Gleam: שליחת בקשת http עם אימות בסיסי בתכנות מחשבים"
simple_title:         "שליחת בקשת http עם אימות בסיסי בתכנות מחשבים"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי היא פעולה שמאפשרת למתכנתים לשלוח בקשות לשרתים עם זיהוי משתמש וסיסמה. זה מאפשר למתכנתים להשתמש בשיטת אימות מעולה עבור יישומים דיגיטליים שמעוניינים לשמור על אבטחת המידע של משתמשים.

איך לעשות:
כאשר משלחים בקשת HTTP עם אימות בסיסי, עליכם להוסיף כותרת מתאימה לבקשה שלכם, שמכילה את שם המשתמש והסיסמה. הנה דוגמא:

```Gleam
let headers = List.of(("Authorization", "Basic dXNlcjpwYXNzd29yZA=="))
let request = Http.request()
                  |> Http.with_method("GET")
                  |> Http.with_url("https://api.example.com/users")
                  |> Http.with_headers(headers)
```

בכתובת של האתר, פשוט החליפו "user" עם המשתמש שלכם ו"password" עם הסיסמה שלכם. אתם יכולים לקבל את הצורה המעודפת של כותרת Authorization בהתאם לדרישות ה-HTTP שלכם.

מעומק:
שיטת האימות הבסיסית היא שיטה ישנה לזיהוי משתמש וסיסמה, שנוצרה עבור התקשורת HTTP הראשונית. המיוחד בשיטת אימות זו הוא שהיא משתמשת במידע גלוי לפני שהוא מוצפן, לכן לעתים קרובות נמצאת מחסור באבטחת המידע. אחד הבידולים הפופולריים לשיטת אימות בסיסית הוא OAuth, המאפשר שיתוף מידע בצורה מאובטחת יותר.

קישורים נוספים:
למידע נוסף על שליחת בקשת HTTP עם אימות בסיסי, ניתן להצטרף לקהילת הג'לאם Gleam Discord server או לעיין במדריך הרשמי של הג'לאם.
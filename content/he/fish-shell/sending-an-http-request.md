---
title:                "Fish Shell: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה

מכיוון שפיתוח אפליקציות מחייב שימוש באינטרנט וכתיבת קוד כדי לשלוח בקשות HTTP אל שרתים, זהו נושא חשוב ללמוד עבור מתכנתי Fish Shell.

## כיצד לעשות זאת

תחילה נצטרך להתקין את חבילת HTTPie בפיתוח האתר או היישום שלנו. לאחר התקנה זו, נוכל להשתמש בפקודה הבאה כדי לשלוח בקשת GET לכתובת URL מסוימת:

```
fish -c "http GET https://example.com"
```

כדי לשלוח בקשות אחרות כמו POST, PUT או DELETE, באפשרותנו להוסיף את הפרמטרים המתאימים לפקודה זו. למשל, כדי לשלוח בקשת POST עם נתונים לכתובת URL ספציפית, נוכל להשתמש בפקודה הבאה:

```
fish -c "http POST https://example.com data='foo'"
```

בתוך כל פקודה נוכל למצוא את התגובה שקיבלנו מהשרת ולהדפיס אותה. נוכל לעשות זאת על ידי שימוש בפקודת echo כדי להדפיס את הכתובת URL שקיבלנו תשובה ממנה, או להשתמש בפקודת tr כדי לסנן את התגובה ולהדפיס רק את המידע הרלוונטי. לדוגמה:

```
fish -c "http GET https://example.com | echo 'Response from example.com' | tr -d '{' | tr -d '}')"
```

## מגעים עמוקים

כעת שהשתמשנו בפקודת HTTPie בפעם הראשונה, נוכל לעצב אותה ולהתאים אותה לסגנון הכתיבה החביב עלינו. עבור כל פקודת HTTP, נוכל להשתמש בפרופילים כדי לשמור על הפרמטרים הנדרשים עבור כל פעמים שנרצה לשלוח בקשה. נוכל לשנות את הפרופיל הנוכחי באמצעות פקודת CURL_CONFIG ולהוסיף או למחוק פרמטרים בפרופיל. לדוגמה:

```
fish -c "export CURLIE_PROFILE=myprofile; http GET https://example.com"
```

למיד
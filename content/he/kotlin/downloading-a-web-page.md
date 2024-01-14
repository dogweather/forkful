---
title:                "Kotlin: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## למה

למה צורך להוריד עמוד אינטרנט? הרבה פעמים אנו רוצים להצטייד במידע מסוים מאתר כדי להשתמש בו להמשך עבודה או לבדוק משהו. הורדת עמוד דרך קוד שפת ווב מאפשרת לנו לשאוב מידע בקלות ומהירות מכל אתר שברצוננו.

## איך לבצע

נתחיל עם קוד פשוט שיעזור לנו להוריד עמוד דרך קוד שפת Kotlin. נתחיל בשליפת האתר שנרצה להוריד באמצעות פקודת `URL` וכתובת האתר. לדוגמה, נבקש להוריד את עמוד הבית של גוגל:

```Kotlin
val url = URL("https://www.google.com")
```

לאחר מכן, נהפוך את הכתובת לחיבור מכווץ באמצעות פקודת `openConnection()` ונקרא את הטקסט שמתקבל באמצעות `bufferedReader()`:

```Kotlin
val connection = url.openConnection()
val text = connection.getInputStream().bufferedReader().use { it.readText() }
```

וכדי להדפיס את הטקסט למסך, נשתמש בפקודת `println()` ונעטוף את הטקסט בסימני גרש כדי לשמור על התבנית של המידע:

```Kotlin
println("$text")
```

כעת נריץ את הקוד ונוכל לראות את התוצאה בקונסולה. במקרה שלנו, נוכל לראות את הטקסט של עמוד הבית של גוגל.

## נחקיר עמוק

כעת שדעתנו על איך להוריד עמוד דרך קוד שפת Kotlin, נרצה לחקור עמוק יותר ולשפר את הקוד שכתבנו. נתחיל בשימוש בפקודת `readText()` כדי לקרוא את הטקסט שמתקבל מהאתר ישירות במקום להשתמש ב `bufferedReader()`. זה יקצר וישמור מקום בקוד:

```Kotlin
val connection = url.openConnection()
val text = connection.getInputStream().reader().readText()
```

נוכל גם להוסיף מסנן לכתובת האתר שברצוננו להוריד, למשל להציג את כל הק
---
title:                "Swift: כתיב אותיות ראשיות במחרוזת"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

יצירת כותרות באמצעות תכנות Swift

## למה

במאמר זה נלמד כיצד ליצור כותרות מלל בתוך תוכניות שפת תכנות Swift. נצטרך את זה במקרים בהם נרצה להדגיש מלל מסוים בתוך אפליקציה או משחק. זה יתן לנו את היכולת לגרום למשתמשים שלנו לשים לב לנקודה חשובה ולשם.

## לאנפול

ליצור כותרת גדולה יותר בתוך תוכנית שפת תכנות Swift יכול להשתמש בפונקציה capitalize כדי להגדיל את האות הראשונה שמופיעה במחרוזת שיש לה את האות הראשונה בכיתה או אינו מבחין לא נופלים.

```Swift
print("swift".capitalize)
output: Swift
```

כדי לכתוב את האות הראשונה עם אות גדולה בתוך כל מחרוזת נשתמש בפונקציה .uppercased().

```Swift
print("hello".uppercased())
output: HELLO
```

אפשר להשתמש בשתי הפונקציות יחד כדי לכתוב את האות הראשונה וגם כל האותיות הנמצאות במחרוזת עם אות גדולה.

```Swift
print("good bye".capitalize.uppercased())
output: Good bye
```

## התנסות עמוקה

הפונקציות capitalize ופונקציות .uppercased() בנויות על התכון הנמלצות לנו במאמר זה. תיכרסו כיצד לאחזר את המחרוזת שלנו בפורמט שבו אנחנו רוצים על ידי שימוש בפונקציות שללמדנו.

שימוש בפונקציות capitalize או uppercased לא רק מאפשר לנו ליצור כותרות בתוך תוכניות שפת תכנות Swift, אלא גם כלי עזר רב לכתיבת תחביר הנוח לקריאה עבור המשתמשים שלנו.

## ראו גם

- [Apple - NSString](https://developer.apple.com/documentation/foundation/nsstring)
- [Swift - String Methods](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID426)
- [هيكوبارا by Hikobara (GitHub)](https://github.com/hikobara/hikobara.github.io)
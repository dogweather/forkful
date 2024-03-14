---
date: 2024-01-26 03:44:30.397420-07:00
description: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05E9\u05DE\u05E2\u05D4 \u05E1\u05D9\
  \u05DC\u05D5\u05E7 \u05E9\u05DC \u05E1\u05D9\u05DE\u05E0\u05D9 \u05D4\u05E6\u05D9\
  \u05D8\u05D5\u05D8 \u05E9\u05DE\u05E7\u05D9\u05E4\u05D9\u05DD \u05D0\u05EA \u05D4\
  \u05EA\u05D5\u05DB\u05DF. \u05D0\u05E0\u05D5 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E0\u05E7\u05D5\u05EA \u05E7\u05DC\u05D8\
  \u05D9\u05DD, \u05DC\u05D4\u05DB\u05D9\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05DC\u05D0\u05D7\u05E1\u05D5\u05DF, \u05D0\u05D5 \u05DC\u05D4\u05D9\u05E4\u05D8\
  \u05E8 \u05DE\u05E2\u05D9\u05E6\u05D5\u05D1 \u05D8\u05E7\u05E1\u05D8 \u05DE\u05D9\
  \u05D5\u05EA\u05E8 \u05E9\u05E2\u05DC\u05D5\u05DC\u2026"
lastmod: '2024-03-13T22:44:39.886325-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05E9\u05DE\u05E2\u05D4 \u05E1\u05D9\u05DC\
  \u05D5\u05E7 \u05E9\u05DC \u05E1\u05D9\u05DE\u05E0\u05D9 \u05D4\u05E6\u05D9\u05D8\
  \u05D5\u05D8 \u05E9\u05DE\u05E7\u05D9\u05E4\u05D9\u05DD \u05D0\u05EA \u05D4\u05EA\
  \u05D5\u05DB\u05DF. \u05D0\u05E0\u05D5 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05E0\u05E7\u05D5\u05EA \u05E7\u05DC\u05D8\u05D9\
  \u05DD, \u05DC\u05D4\u05DB\u05D9\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DC\
  \u05D0\u05D7\u05E1\u05D5\u05DF, \u05D0\u05D5 \u05DC\u05D4\u05D9\u05E4\u05D8\u05E8\
  \ \u05DE\u05E2\u05D9\u05E6\u05D5\u05D1 \u05D8\u05E7\u05E1\u05D8 \u05DE\u05D9\u05D5\
  \u05EA\u05E8 \u05E9\u05E2\u05DC\u05D5\u05DC\u2026"
title: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

הסרת מרכאות ממחרוזת משמעה סילוק של סימני הציטוט שמקיפים את התוכן. אנו עושים זאת כדי לנקות קלטים, להכין נתונים לאחסון, או להיפטר מעיצוב טקסט מיותר שעלול להפריע לעיבוד הנתונים.

## איך לעשות:

Swift מאפשרת לך לטפל בעבודת הסרת הציטוטים בצורה נוחה מאוד. הנה דוגמה מהירה באמצעות `replacingOccurrences(of:with:)`, שעושה בדיוק מה שנשמע כאילו—מחליפה חתיכות טקסט במשהו אחר, או בכלום כלל.

```swift
var quotedString = "\"This is a 'quoted' string.\""
let unquotedString = quotedString.replacingOccurrences(of: "\"", with: "")
print(unquotedString) // This is a 'quoted' string.

// מתמודדים עם מרכאות יחידות? פשוט שנה את מונח החיפוש.
quotedString = "'Here's another example.'"
let singleQuoteRemoved = quotedString.replacingOccurrences(of: "'", with: "")
print(singleQuoteRemoved) // Heres another example.
```

הפלט יהיה מחרוזות חופשיות ממרכאות, מוכנות לכל מה שתתכננו לעשות הלאה.

## חקירה עמוקה

אנו "נוקים" מחרוזות כאלו מאז ראשית ימי התכנות. בימים הראשונים, זה היה יותר עניין של שימור זיכרון יקר ומניעת שגיאות תחביר בעיבוד קלטים. קדימה להיום, וזה עניין של היגיינת נתונים טובה—במיוחד כשמתמודדים עם JSON או מכינים מחרוזות לעבודה בבסיסי נתונים. מרכאות שנשארות בטעות יכולות להכשיל שאילתות SQL מהר יותר מאשר נאמר "שגיאת תחביר".

אלטרנטיבות? ובכן, אם אתה מוצא את `replacingOccurrences(of:with:)` מעט מדי וניל, ייתכן שתחקור את עולם הביטויים הרגולריים לתבניות יותר מורכבות או כאשר אתה רוצה להסיר מרכאות רק במקומות מסוימים. כיתת `NSRegularExpression` בSwift היא חבר שלך כאן. אך זכור, regex יכול להיות חרב פיפיות—חזק, אך לעיתים גם יתר על המידה.

מבחינת יישום, `replacingOccurrences(of:with:)` היא שיטה שמסופקת על ידי `String` בSwift, שקוראת בפנים לפונקציות יותר מורכבות למניפולציה של מחרוזות שמתמודדות עם יוניקוד ופרטיות נוספות של עיבוד טקסט מודרני. זו אחת מהעסקאות ה"פשוטות במבט ראשון, מורכבות מתחת למכסה" שSwift מתמודדת עבורך כך שלא תצטרך לעשות זאת.

## ראה גם

למידע נוסף על מניפולציות של מחרוזות בSwift:

- The Swift Programming Language (Strings and Characters): [תיעוד של Swift.org](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- NSRegularExpression: [תיעוד של מפתחי Apple](https://developer.apple.com/documentation/foundation/nsregularexpression)

ואם אתה עכשיו סקרן לגבי ביטויים רגולריים ורוצה לבדוק את התבניות שלך:

- Regex101: [מבחן ומנפה שגיאות Regex](https://regex101.com)

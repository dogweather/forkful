---
title:                "Swift: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/parsing-html.md"
---

{{< edit_this_page >}}

## למה
מידי פעם נתקלים במצבים בהם אנחנו רוצים לקבל מידע מאתר אינטרנט באופן אוטומטי. למשל, כאשר אנחנו מנסים לאתר מחירים של מוצרים מסוימים או לבדוק זמינות של מוצר בחנויות שונות. במצבים כאלה נדרש לקרוא ולנתח את קוד ה-HTML של האתר כדי להשיג את המידע הרלוונטי. בתוך המאמר הזה, נלמד כיצד לעשות זאת בעזרת שפת תכנות סוויפט.

## איך לעשות זאת
אחד הדרכים הפשוטות לקרוא ולנתח קוד HTML הוא באמצעות ספריית 'SwiftSoup'. נוכל להתקין אותה בעזרת ניהול התוכניות שלנו, כמו 'CocoaPods'. לאחר התקנה, נוכל ליצור אובייקט מסוג 'Document' ולהפעיל עליו את הפעולות הרלוונטיות כדי לקרא ולנתח את הקוד של האתר הרצוי. כדי להדגים את השימוש בספרייה זו, ניצור אפליקציה בסוויפט ונשתמש במחלקה המבוססת על 'SwiftSoup' כדי לקרוא את הקוד הפשוט של האתר https://www.google.com/ ולהדפיס את כותרת האתר הראשית בקונסול.

```Swift
import SwiftSoup

let url = URL(string: "https://www.google.com/")
do {
    let html = try String(contentsOf: url!)
    let document: Document = try SwiftSoup.parse(html)
    let title: Element = try document.select("title").first()!
    print(try title.text())
} catch {
    print("Error!")
}
```

הפלט של הקוד הנ"ל יהיה: "Google".

## לרבות בעמקים
אם נרצה לקחת את ההתייחסות שלנו לפיסול HTML לרמה הבאה, נוכל להשתמש בכתיבת תבניות (regex) כדי למצוא ולשנות את הטקסט הרלוונטי באתר. בנוסף, ניתן להשתמש בכלי תיקונו (parser) של ה-HTML עבור שפת סוויפט, שיכול לפזר את הקוד של האתר לעץ
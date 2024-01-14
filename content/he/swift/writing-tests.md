---
title:                "Swift: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/writing-tests.md"
---

{{< edit_this_page >}}

בעלי התכניתים הבריטיים הסטנדרטיים יעדיפו לבדוק את הקוד שלהם לפני שהם אוהבים לשחרר אותו לעולם. אבל עם תכניות בשפות תכנות חמות כמו Swift, לסדר ביצועי מבחן נעילה מתחברים יותר ויותר בעבריית משתמשי המחשבות שלהם. מאמר זה יסביר לך לאזור איך אתה יכול לכתוב מבחן נעילה בשפת Swift כדי לוודא שהתכנית שלך רצה כפי שצריך.

## למה

מבחן נעילה הוא בעצם תכליתי כדי לוודא שתכנית שלך רצה בסדר שמתאים למה שאתה מצפה לו. זה מוגדל השגת תוכנות שאינן מדוייקות ואינן נגישות.

## איך

#### Swift מבחן נעילה דוגמא עם תלתמימדי

כאן קוד ככל שנוכל בשיכולת הייכולה שלכם:

* ** אנא התא המשך לבצע בדיקת פינוי ו……
* **אנא התא המשך לבצע בדיקת פינוי לפני שתיאומלפת, אלא זה יפריד את הדרכונים

<details>

```Swift
class Tutorial {
    var title: String
    var author: String
    var isIncomplete: Bool

    init(title: String, author: String, isIncomplete: Bool) {
        self.title = title
        self.author = author
        self.isIncomplete = isIncomplete
    }
}

func runTutorialReport(tutorial: Tutorial) {
    if tutorial.title.isEmpty || tutorial.author.isEmpty {
        print("*** ERROR: There is no title or author printed.")
    } else if tutorial.isIncomplete {
        print("The tutorial '\(tutorial.title)' by \(tutorial.author) needs to be completed.")
    } else {
        print("The tutorial '\(tutorial.title)' is complete. Thanks \(tutorial.author)!")
    }
}

let myTutorial = Tutorial(title: "Learn Swift", author: "John Doe", isIncomplete: true)
runTutorialReport(tutorial: myTutorial)

let myOtherTutorial = Tutorial(title: "iOS Development", author: "Jane Smith", isIncomplete: false)
runTutorialReport(tutorial: myOtherTutorial)
```

Output:

```
The tutorial 'Learn Swift' by John Doe needs to be completed.
The tutorial 'iOS Development' is complete. Thanks Jane Smith!
```

</details>

## מהומה עמוקה

הגולל הזה הוא נשמע נפלאו מספק לך יכלילים עמוקים נוספים על כתונת כיום כתגישבימיק הקונעב ובחן כתונת תףניג עם גישביום
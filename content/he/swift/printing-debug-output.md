---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?
יציאת ניפוי שגיאות היא שיטה שמסייעת למתכנתים לזהות ולאתר בעיות או שגיאות בתוכנית שלהם. מתכנתים משתמשים בכך כדי להבין את ההתנהגות של קוד מסוים, לאטר בעיות על מנת לתקן שגיאות ולאופטים את התוכנית.

## איך לעשות:
בלשון התכנות Swift, ניתן להדפיס יציאה של ניפוי שגיאות על ידי שימוש בפונקציה `print()`.

```Swift
let name = "Swift"
print("Hello, \(name)")
```

הקוד הנ"ל ייצא את המחרוזת "Hello, Swift" לחלון ה-Outputs.

## צלילה עמוקה:
הדפסת יציאת ניפוי היא שיטה שהתפתחה מאז שהתחילו לכתוב קוד. זה היה דרך יעילה למתכנתים לבחון ולבדוק את הריצה של קוד התוכנית שלהם בזמן אמת. החלופות כוללות שימוש במנגנון Debug של Xcode או שימוש במנגנון הניפוי של המערכת המראה שגיאות בזמן ריצה.

## ראה גם:
[SOSwift: Debug Utilities](https://www.sosoftware.io/swift/debugging/utilities): Guides, tips, and code examples for debugging Swift code.

[Xcode: Debugging tools](https://developer.apple.com/documentation/xcode/debugging_your_app): Documentation and getting-started guides for using Xcode's built-in debugging utilities. 

[Swift.org: Debugging](https://swift.org/debugging): Official documentation and discussions about debugging in Swift.
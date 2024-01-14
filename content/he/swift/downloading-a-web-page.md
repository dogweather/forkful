---
title:                "Swift: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# למה

למה לחצתם על הכתבה הזו? יכול להיות שאתם מתעניינים בתכנות בשפת Swift או שרוצים ללמוד כיצד להוריד דף אינטרנט באמצעות קוד.

## איך לעשות זאת

הנה כמה דוגמאות של קוד ופלט תוצאה בשפת Swift:
```Swift
if let url = URL(string: "https://www.example.com"){
	let task = URLSession.shared.dataTask(with: url, completionHandler: { (data, response, error) in
		guard let data = data, error == nil else {
			print("Error: \(error!)")
			return
		}
		if let htmlString = String(data: data, encoding: .utf8){
			print(htmlString)
		}
	})
	task.resume()
}
```

כאן אנו משתמשים בפונקציית dataTask של URLSession כדי להוריד את הדף מהאינטרנט ולקרוא את המידע שבו כטקסט. ניתן להקליד גם על NSURLSession, אבל זה מתאים יותר לאפליקציות שעובדות ב- iOS 9 ומעלה.

## Deep Dive

כדי להבין כיצד תכנת Swift מושג ומשתמש במספר פרטים כדי להוריד דף אינטרנט, כדאי להתעמק וללמוד את פונקציות האחראיות על ההורדה והטיפול במידע. פונקציית URLSession, לדוגמה, מתאפיינת באפשרות להוריד דפים מספרים בו זמנית וכן בכך שמאפשרת לתפעול את התהליכים ברקע.

# לראות גם

למידע נוסף על תכנות בשפת Swift והורדת דפי אינטרנט, ניתן לקרוא את המאמרים הבאים:
- [כתיבת קוד בשפת Swift למתחילים](https://www.raywenderlich.com/502-introducing-swift-and-its-stdlib)
- [כיצד לגשת לקבצי רשת באמצעות URLSession בשפת Swift](https://www.raywenderlich.com/567-urlsession-tutorial-getting-started)
- [מדריך למתחילים על NSURLSession בשפת Swift](https://www.appcoda.com/urlsession-swift/)
- [פעולות רכיבות השכבת רשת של NSUrlsession](https://developer.apple.com/reference/foundation/urlsession)
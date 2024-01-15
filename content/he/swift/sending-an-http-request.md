---
title:                "שליחת בקשת http"
html_title:           "Swift: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

### למה

אנשים משתמשים בקריאת בקשת HTTP כאמצעי לגשת למידע שנמצא בשרתים מרוחקים. זה מאפשר להם לקבל תגובה מהר יותר ולשתף מידע עם מספר שרתים באופן מקבילי.

### איך לעשות זאת

כדי לשלוח בקשת HTTP באמצעות שפת סוויפט, ניתן להשתמש במחלקה המובנית URLSession ולבחור בין מתודות כמו `dataTask` או `uploadTask` על פי הצורך. להלן מספר דוגמאות קוד והתוצאות המצורפות.

```Swift
// דוגמא 1: שליחת בקשת GET
let url = URL(string: "https://www.example.com/api/data")
let task = URLSession.shared.dataTask(with: url!) { data, response, error in
	guard let data = data, error == nil else {
		print("שגיאה בתהליך קבלת המידע: \(error?.localizedDescription)")
		return
	}
	let response = response as? HTTPURLResponse
	print("קוד תגובה: \(response!.statusCode)")
	print("תוכן התגובה: \(String(data: data, encoding: .utf8)!)")
}
task.resume()
```

תוצאה:
```
קוד תגובה: 200
תוכן התגובה: {"name": "John Doe", "age": 30}
```

```Swift
// דוגמא 2: שליחת בקשת POST עם גוף נתונים
let url = URL(string: "https://www.example.com/api/addUser")
var request = URLRequest(url: url!)
request.httpMethod = "POST"
request.setValue("application/json", forHTTPHeaderField: "Content-Type")
let jsonBody = ["name": "Jane Smith", "age": 25]
let jsonData = try JSONSerialization.data(withJSONObject: jsonBody, options: .prettyPrinted)
request.httpBody = jsonData
let task = URLSession.shared.dataTask(with: request) { data, response, error in
	guard let data = data, error == nil else {
		print("שגיאה בתהליך קבלת המידע: \(error?.localizedDescription)")
		return
	}
	let response = response as? HTTPURLResponse
	print("קוד תגובה: \(response!.statusCode)")
	print("תוכן התגובה: \(String(data: data, encoding: .utf8)!)")
}
task.resume()
```

תוצאה:
```
קוד תגובה: 201
תוכן התגובה: {"message": "המשתמש Jane Smith נוסף בהצלחה!"}
```

### חקירה מעמיקה

לשלוח בקשת HTTP ניתן להזדהות עם כל סוג URL ולהשתמש בכל טיפוסי HTTP מתאימים. ניתן גם לציין כתובת URL מלאה כארגומנט לכל המתודות הראשיות של URLSession.

### ראו
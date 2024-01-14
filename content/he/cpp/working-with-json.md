---
title:                "C++: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/working-with-json.md"
---

{{< edit_this_page >}}

ניצולי תכנות בסיסיים: עבודה עם JSON

## למה?

JSON הוא פורמט פתוח וקל לקריאה שמיועד לביצוע ייעודים שונים בתכנות. העבודה עם JSON נחשבת לחיונית בעבודה עם נתונים מבוססי אינטרנט וגם כחלק מיסודי של פיתוח יישומים ותוכניות מחשב.

## כיצד לעבוד עם JSON ב-C++?

ניתן לפנות לפונקציות המיועדות לעבודה עם JSON כמו `parse()` ו- `stringify()` על מנת לייצר או לקרוא נתונים מתוך פורמט JSON. ניצולי תכנות דומו לטיפול ב-Object ואילו תכנות כיווניים דומות לפעולות בתוך קבצי תבניות.

לדוגמה, אם נרצה ליצור משתנה מסוג JSON נוכל להשתמש בקוד הבא:

```C++
#include <iostream>
#include <json/json.h>

using namespace std;

// יצירת משתנה מתוך פורמט JSON
Json::Value myVariable;

// הגדרת ערכים בתוך המשתנה
myVariable["name"] = "John";
myVariable["age"] = 25;
myVariable["city"] = "Tel Aviv";

// הדפסת נתוני המשתנה
cout << "Name: " << myVariable["name"].asString() << endl;
cout << "Age: " << myVariable["age"].asInt() << endl;
cout << "City: " << myVariable["city"].asString() << endl;
```

תוצאה:

```
Name: John
Age: 25
City: Tel Aviv
```

ניתן גם לקרוא נתונים מתוך פורמט JSON באמצעות פעולה דומה, כפי שניתן לראות מן הקוד הבא:

```C++
#include <iostream>
#include <json/json.h>

using namespace std;

// התחברות לשרת JSON
Json::Value server = "http://www.example.com/json";

// יצירת משתנה כדי לאחסן את הנתונים שיוחזרו
Json::Value results;

// קריאת הנתונים מהשרת
Json::Reader reader;
bool parsingSucceeded = reader.parse(server, results);

// בדיקה האם קריאת הנתונים הצליחה
if (parsingSucceeded) {
  // הדפסת התוצאות
  cout << "First Name: " << results["firstName"].asString() << endl;
  cout << "Last Name: " << results["lastName"].asString() << endl;
  cout << "Age: " << results["age"].asInt() << endl;
}
```

תוצאה:

```
First Name: David
Last Name: Cohen
Age:
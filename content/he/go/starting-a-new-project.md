---
title:                "התחלת פרויקט חדש"
html_title:           "Clojure: התחלת פרויקט חדש"
simple_title:         "התחלת פרויקט חדש"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה ולמה?
אתחול פרויקט חדש הוא הבחירה ליצור מוצר חדשני משום מקום. מתכנתים עושים זאת כדי ליישם רעיונות פורצי דרך, להתמודד עם אתגרים חדשים ולהתפתח כמקצוענים.

## איך לעשות:
אגרסיבי פישוף של שפת Go:

```Go
package main

import "fmt"

func main() {
	fmt.Println("Shalom, Olam!")
}
```

הפלט יהיה:

```Go
"Shalom, Olam!"
```

חזור ירוק לראש הפישוף, כדי ליצור תכניות Go מבפנים:

```Go
package main

import (
	"fmt"
	"net/http"
)

func helloWorld(w http.ResponseWriter, r *http.Request){
	fmt.Fprintf(w, "Shalom, Olam!")
}

func main() {
	http.HandleFunc("/", helloWorld)
	http.ListenAndServe(":8080", nil)
}
```

## צלילה עמוקה:
נוצרה ב-2007 ב- Google, שפת תכנות Go הייחודית מכיוון שהיא שולבה בסגנון תכנות מבני מן הוותיקים עם פתרונות עדכניים למכשולים אשר התקיימו. אלטרנטיבות כמו JavaScript, Python או Java קיימות, אך Go מתאפיינת בקוד מינימליסטי, ביצועים מהירים ומסוף נחמד. במהלך הקמת פרויקט, משתבחים מפיצרים מרכזיים כמו מודולים חיצוניים, סבלנות קונקורנטית וניהול זיכרון טוב.

## ראה גם:
- [מדריכים ל-Go באתר הרשמי של Go](https://golang.org/doc/)
- [מסמכי ה-API של Go](https://golang.org/pkg/)
- [הפרויקט המקורי של Go אצל Google](https://github.com/golang/go)
- [מדריך התקנה של שפת Go](https://golang.org/doc/install)
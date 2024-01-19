---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
הורדת דף אינטרנט היא הפעולה שבה מחשב אוסף את כל המידע שנשלח מהשרת ומארגן אותו בצורה שניתן לקרואה ולהבין. מתכנתים מורידים דפים מהאינטרנט כדי לראות מה מציג האתר, לאסוף מידע, ולבחון את דפי התוכן.
 
## איך ל:
ראשית, עלינו לייבא את החבילה  "net/http". לאחר מכן נבצע בקשת GET לדף האינטרנט.

```Go
package main

import (
	"io/ioutil"
	"log"
	"net/http"
)

func main() {
	resp, err := http.Get("http://example.com")
	if err != nil {
		log.Fatalln(err)
	}

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatalln(err)
	}

	log.Println(string(body))
}
```
עכשיו, אם נריץ את התוכנית, נוכל לראות את תוכן הדף של "http://example.com" מודפס במסוף.

## צלילה עמוקה
המסגרת הראשונית לגריפת דף אינטרנט באמצעות קוד הייתה חבילת `http` של Go שנוצרה כחלק מגרסה הראשונה של השפה. למרות שניתן להוריד דף האינטרנט בשיטות אחרות, כמו למשל בשימוש ב-Sockets מהשלב התחתון, השיטה של Go הפכה לאפשרות המועדפת בגלל הנוחות והשלמות המערכתית. פרטים על איך Go מבצע אותו הם מחוץ להיקף המאמר הזה, אבל הם בהחלט מרתקים ומאתגרים.

## ראה גם
- הספר "The Go Programming Language" - https://www.gopl.io/
- המדריך הרשמי של Go לחבילת "net/http" - https://golang.org/pkg/net/http/
- אתר האינטרנט הרשמי של Go - https://golang.org/

אני מקווה שהמאמר הזה היה מועיל ומעניין לך!
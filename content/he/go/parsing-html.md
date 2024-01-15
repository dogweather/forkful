---
title:                "פירוק קוד HTML"
html_title:           "Go: פירוק קוד HTML"
simple_title:         "פירוק קוד HTML"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/parsing-html.md"
---

{{< edit_this_page >}}

## מדוע

אם אתם מתכנתים, ייתכן שתגיעו למצבים בהם תצטרכו לטפל בנתונים שמגיעים מקוד HTML. בג'ו, זה די קל להפוך את התהליך הזה למהיר ויעיל.

## איך לעשות זאת

### התקנת החבילה

לפני שניכנס לפנייה ניתן לקחת ארגז הכלים של `http` ולהביא את המערכת וכן `html` אבל השיטה המומלצת לעשות את זה היא ע"י התקנת החבילה `golang.org/x/net/html`.

כדאי לוודא שהחבילה מתוקנת בצורה נכונה ואם לא כך, אז לשלוח `go get` כמו תמיד.

### השתמש ב`Parse`

עבור תחילת הפעילות של הקוד שכתבנו, נגדיר משתנה שנקרא `doc` שיכיל את הרצועה של התשתית שהוספנו ברגע הקודם.

```Go
doc, err := html.Parse(resp.Body)
```

במקרה הנוכחי, המתודה של `Parse` תהיה נמשכת מהתשובת האתר המבוקש המופיע במשתנה `resp`.

### מציאת אלמנטים

בכוונה, אנחנו נחפש את האלמנט הרצוי באמצעות השיטה `findLinks`.

```Go
func findLinks(n *html.Node) []string {
	var links []string
	// רק אם זה תנאי הזה
	if n.Type == html.ElementNode && n.Data == "a" {
		for _, a := range n.Attr {
			if a.Key == "href" {
				links = append(links, a.Val)
			}
		}
	}
	// זה יוצא מכל השאר
	for c := n.FirstChild; c != nil; c = c.NextSibling {
		links = append(links, findLinks(c)...)
	}
	return links
}
```

זה לא משנה מה זה HTML נראה להכריח את השיטה `findLinks` לרדת לילדים של `Node` ולהתחיל לעבוד רק כשהיא מגיעה לאיבר הרלוונטי.

בסוף הקוד מחזיר רשימה של אופני מחיקה עבור סוגי נתונים HTML כגון.

Url x, url y, url z, url3

הנה פלט לקריאה של הפונקציה הקודמת:

```
$ go run fetching_urls.go

https://www.yourwebsite.com
http://www.example.com
http://www.example2.com
```

### השת
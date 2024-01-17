---
title:                "כתיבת בדיקות"
html_title:           "Go: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות היא תהליך שבו מתבצעים בדיקות על קטעי קוד כדי לוודא שהם עובדים כפי שצופו. תהליך זה חיוני לתוכניות תוכנה כדי להבטיח פעולה תקינה ולמנוע באגים ובעיות בפרסום או בהרצת התוכנית.

## איך לבצע:
```Go
func Add(x, y int) int {
	return x + y
}

func TestAdd(t *testing.T) {
	sum := Add(3, 5)
	if sum != 8 {
		t.Errorf("expected 8, got %d", sum)
	}
}
```

## טביעת רגל:
כתיבת בדיקות בתוכניות תוכנה לא היתה מקובלת לפני התקווה. אך עם התפתחות התוכניות והצורך באיכות, בדיקות נהפכו לחלק חיוני מתהליך הפיתוח. ישנן כמה כלים נפוצים אחרים לביצוע בדיקות כמו Pytest ו- JUnit.

## ראו גם:
- [כתיבת בדיקות ב- Go ספר המדריך הרשמי](https://golang.org/doc/code.html#Testing)
- [בדיקות יחידה עם Pytest](https://docs.pytest.org/en/latest/)
- [מדריך למתכנתים על JUnit](https://www.vogella.com/tutorials/JUnit/article.html)
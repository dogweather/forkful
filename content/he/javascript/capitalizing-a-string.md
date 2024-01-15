---
title:                "כתיבה באותיות רישיות במחרוזת"
html_title:           "Javascript: כתיבה באותיות רישיות במחרוזת"
simple_title:         "כתיבה באותיות רישיות במחרוזת"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה

אנשים ישתתפו בשכתוב באופן כללי בתכנן בשפה התכנות Javascript בגירסה הנוכחית היא על מנת להציג לך את הברירה האישית שלי

,ישנן מגוון רחב של טכניקות ומתודות שניתן להשתמש בהן בכדי לשפר את קוד ה Javascript שכתבתם. דבר אחד שנמצא כללי בכמעט כל קוד הוא שהאות הראשונה כל מלה צריכה להיות באות גדולה. אם אתה מנסה למצוא את הדרך הטובה ביותר למכן את הטקסט הזה דווקא יכול להיות מאוד מועיל עבורך.

## איך לעשות זאת

הצורה הכי פשוטה להכן אות כל מלה במחרוזת היא להשתמש במתודת השבירה המוכנה של Javascript. כאן אנו משתמשים במתודת `toUpperCase` שתמיד תחזיר מפתח לאות גדולה. הנה דוגמא לקוד:

```Javascript
let string = "this is an example string";
let capitalizedString = string.charAt(0).toUpperCase() + string.slice(1);
```

כאן אנו משתמשים בפונקציה `charAt` כדי לגשת לתו הראשון של המחרוזת, מעקוב אחר זה עם השימוש במתודה `toUpperCase` כדי לשכן את התו לאות גדולה. לבסוף, אנו משתמשים במתודה `slice` כדי לחתוך את המחרוזת מהסימן האחרי הראשון, ולהחזיר את המחרוזת החדשה שבה התו הראשון שכן מיותר.

## חקירה עמוקה

כיצד מתודות כמו `toUpperCase` עובדות? מאחר ו Javascript היא שפת תכנון בת אופי דינמי, כמו הגוף האנושי, זה משמעו שניתן להוסיף ["תכונות גזעיות"](https://en.wikipedia.org/wiki/Prototype-based_programming) לכל אובייקט בזמן הריצה. לכן, כאשר אנו משתמשים במתודת `toUpperCase` על אובייקט מסוג מחרוזת, היא מזדקפת לתוך האובייקט ה
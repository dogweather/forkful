---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# שלוף דף אינטרנט באמצעות Fish Shell

## מה ולמה?
ירידת דף אינטרנט היא תהליך שבו המחשב שלכם מחליף נתונים עם שרת ומטמין את הדף. תכנתים משתמשים בו כדי לאסוף מידע, לבדוק מצב שרת, או ליצור עזרים לאוטומציה.

## איך לעשות:
הנה כמה דוגמאות קוד עם פלטים משלהם:

```Fish Shell
# הראשון הוא ירידת טקסט מקום עם wget
function download_page
  wget $argv[1] -q -O -
end
download_page "http://example.com"
```

מסופק דף האינטרנט של `example.com` כטקסט.

## צלילה עמוקה
מסגרת Fish Shell משמעותית צעירה יותר מקונכיות ה-Unix הקלאסיות כמו bash, אך היא מדהימה בקלות השימוש שלה וקלות ההבנה. היום, ישנם חלופות רבות (כמו cURL) ללמידה של דפי אינטרנט, כאשר כל אחת מהן כוללת יתרונות וחסרונות משלה. במהלך תהליכי ההיורדות, התכנה סוכלת להמיר את הנתונים המחזוריים שקבלה לטקסט.

## להבין יותר
כמה מראש: אתרי האינטרנט [Wikipedia article on web scraping](https://he.wikipedia.org/wiki/%D7%A7%D7%A8%D7%99%D7%99%D7%AA%D7%AA_%D7%90%D7%AA%D7%A8%D7%99%D7%9D), מדריך הערה [Official Fish Shell Documentation](https://fishshell.com/docs/current/index.html), והמאמר [HTML, explained](https://developer.mozilla.org/he/docs/Web/Guide/HTML/HTML5).
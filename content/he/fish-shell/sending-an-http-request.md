---
title:                "שליחת בקשת http"
html_title:           "Fish Shell: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה

לכמה אנשים נדרש לשלוח בקשת HTTP? הגישה הזו נהיה חשובה כאשר ברצונם לקבל מידע מתכניות דרך האינטרנט, לשלוח מידע למסד נתונים, או לאפשר למשתמשים לתקשר עם האתר שלכם.

## איך לעשות זאת

כדי לשלוח בקשת HTTP בדרך נוחה וקלה, נוכל להשתמש בפקודת `curl` בתוך פקודת הקשת `Fish Shell`. נלמד איך להשתמש בפקודה ונדגים דוגמאות של פלט מעניין.

```fish
# שלוח בקשה GET לדף הבית של גוגל
curl "https://www.google.com"

# שלוח בקשה POST עם מידע מסוג JSON
curl -X POST \
  -H "Content-Type: application/json" \
  -d '{"username": "admin", "password": "1234"}' \
  "https://api.example.com/login"
```

## חפירה עמוקה

כמו הכל, ישנם יותר מפקודת `curl` לשליחת בקשת HTTP. נוכל להתאים את הבקשה עם מידע נוסף, כגון פרמטרים, כותרות, וכו'. ניידע גם על כלי נוסף, כמו `wget` ו`httpie`, שיכולים להקל על התקשורת עם שרתי האינטרנט.

## ראו גם

- [דוגמאות של `curl`](https://gist.github.com/subfuzion/08c5d85437d5d4f00e58)
- [מדריך לשליחת בקשות HTTP באמצעות `httpie`](https://codeburst.io/httpie-a-manual-for-its-command-line-utilities-9e0a36ac1036)
- [פקודת `download` של `Fish Shell`](https://fishshell.com/docs/3.0/cmds/download.html)
---
date: 2024-01-20 17:55:52.048879-07:00
description: "\u05DE\u05D4 \u05D6\u05D4 \u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\
  \u05D2\u05D5\u05DE\u05E0\u05D8\u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\
  \u05E4\u05E7\u05D5\u05D3\u05D4? \u05D6\u05D4 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA\
  \ \u05E9\u05D1\u05D5 \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05DE\u05E7\u05D1\u05DC\
  \u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\u05D4\u05DE\u05E9\u05EA\u05DE\
  \u05E9 \u05D3\u05E8\u05DA \u05D4\u05D8\u05E8\u05DE\u05D9\u05E0\u05DC. \u05DC\u05DE\
  \u05D4 \u05D6\u05D4 \u05D7\u05E9\u05D5\u05D1? \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E4\
  \u05E9\u05E8 \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05E6\u05D9\u05D4 \u05D2\
  \u05DE\u05D9\u05E9\u05D4 \u05E2\u05DD \u05D4\u05EA\u05D5\u05DB\u05E0\u05D4 \u05E9\
  \u05DC\u05DA \u05D5\u05E7\u05D1\u05DC\u05EA \u05E7\u05DC\u05D8\u2026"
lastmod: '2024-03-13T22:44:40.075714-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D4 \u05D6\u05D4 \u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\
  \u05D5\u05DE\u05E0\u05D8\u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\
  \u05E7\u05D5\u05D3\u05D4? \u05D6\u05D4 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\
  \u05D1\u05D5 \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05DE\u05E7\u05D1\u05DC\u05EA\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\u05D4\u05DE\u05E9\u05EA\u05DE\u05E9\
  \ \u05D3\u05E8\u05DA \u05D4\u05D8\u05E8\u05DE\u05D9\u05E0\u05DC. \u05DC\u05DE\u05D4\
  \ \u05D6\u05D4 \u05D7\u05E9\u05D5\u05D1? \u05DB\u05D3\u05D9 \u05DC\u05D0\u05E4\u05E9\
  \u05E8 \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05E6\u05D9\u05D4 \u05D2\u05DE\
  \u05D9\u05E9\u05D4 \u05E2\u05DD \u05D4\u05EA\u05D5\u05DB\u05E0\u05D4 \u05E9\u05DC\
  \u05DA \u05D5\u05E7\u05D1\u05DC\u05EA \u05E7\u05DC\u05D8\u2026"
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
---

{{< edit_this_page >}}

## What & Why?
מה זה קריאת ארגומנטים משורת הפקודה? זה התהליך שבו תוכנית מקבלת נתונים מהמשתמש דרך הטרמינל. למה זה חשוב? כדי לאפשר אינטראקציה גמישה עם התוכנה שלך וקבלת קלט דינאמי.

## How to:
קוד לדוגמה ופלט דוגמתי תוך שימוש בבלוקי קוד של Fish Shell.

```Fish Shell
function greet
    set name $argv[1]
    echo "שלום, $name"
end

greet אורח
```

פלט:
```
שלום, אורח
```

בדוגמה זו, הפונקציה `greet` מקבלת פרמטר אחד ומדפיסה ברכה.

## Deep Dive
בעבר, קריאת ארגומנטים בקוד לינוקס התבצעה בעיקר ב-Shell Scripts כמו Bash. Fish Shell הקל על התהליך על ידי הצגת פקודות ותחביר פשוט יותר. חלופות כוללות Bash, Zsh ו-Perl, אבל Fish נתפס כיותר קריא ואינטואיטיבי. ב-Fish, ניתן לגשת לארגומנטים דרך המשתנה `$argv`, שהוא רשימה של כל הארגומנטים הנתונים.

## See Also
- [תיעוד Fish Shell](https://fishshell.com/docs/current/index.html)
- [מדריך למתחילים ב-Fish](https://fishshell.com/docs/current/tutorial.html)
- [Stack Overflow: דיונים על Fish Shell](https://stackoverflow.com/questions/tagged/fish)

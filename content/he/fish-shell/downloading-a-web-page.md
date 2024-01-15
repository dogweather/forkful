---
title:                "הורדת דף אינטרנט"
html_title:           "Fish Shell: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## למה
 עבור כדי להעלות אתרי אינטרנט מהר יותר, כדי לקרוא ולעבד מידע מתוך אתרים וכדי ליצור סקריפטים אוטומטיים.

## איך לבצע
כדי להוריד את הדף של אתר האינטרנט, ניתן להשתמש בפונקציונליות המובנת של Shell כגון `wget` או `curl`. למשל:

```Fish Shell
wget http://www.example.com
```

הפקודה הנ"ל תיצור קובץ חדש בשם "index.html" המכיל את כל המידע המופיע בדף של האתר המבוקש.

## מעמקים
כדי לקבל מידע מפורט יותר על היכולות של Shell בנושא הורדת דפי אינטרנט, ניתן לקרוא תיעוד רחב יותר כמו כן ישנם ספריות ופקודות מתקדמות יותר המאפשרות טיפול נתונים בצורה מתוחכמת יותר. אולם, בשימוש פשוט כמו להוריד דף בודד, Shell תהיה דרך מצוינת לטפל בכך.

## ראה גם
- [תיעוד פקודת wget באתר GNU] (https://www.gnu.org/software/wget/manual/wget.html) 
- [תיעוד פקודת curl באתר curl] (https://curl.haxx.se/docs/manpage.html)
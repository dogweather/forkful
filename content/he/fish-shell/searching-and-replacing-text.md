---
title:    "Fish Shell: חיפוש והחלפת טקסט"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה

אז למה כדאי לך ללמוד על חיפוש והחלפת טקסט בסביבת Fish Shell? כי כשמתחילים לתכנת ולעבוד עם טקסטים וכתיבת קוד, חיפוש והחלפה הם כלי חשובים ביותר כדי לשפר את היעילות ולאפשר תיקון שגיאות ושינויים מהירים בקוד.

## איך לעשות

תחת כותרת זו, אני מראה לכם מספר דוגמאות להראות איך אפשר למצוא ולהחליף טקסט בFish Shell. כל קטע קוד מופיע בבלוק "```" כדי להראות דוגמאות של קוד שניתן להריץ ישירות מסביבת הטרמינל.

```
# חפש והחלף מחרוזת מסוימת
echo אחת שתיים שלוש | sed 's/שתיים/שבריים/'

# החלף מחרוזת בעזרת מתאם רגולרי
echo status_200.html | sed 's/status_\(.*\).html/\1/'
```

העלות הפלט הוא:

```
אחת שסלוש שני שלוש|\1
200
```

## חפירה עמוקה

כדי להשתלט על כל האפשרויות ולהיות מומחה בחיפוש והחלפה של טקסט בFish Shell, תוכל לאתר וללמוד יותר על פקודות והגדרות נוספות כמו `sed` ו- `grep`. נהפוך אתכם למומחים!

## ראו גם

כדי ללמוד עוד על חיפוש והחלפת טקסט בסביבת Fish Shell, אני ממליץ על קריאת המדריכים הבאים:

- [The Fish Shell ספרית המדריכים הרשמים](http://fishshell.com/docs/current/tutorial.html)
- [מדריך מהיר לפקודות Sed ו- Grep](https://www.digitalocean.com/community/tutorials/using-grep-regular-expressions-to-search-for-text-patterns-in-linux)
- [מדריך נוסף על מתאם רגולרי עם Fish Shell](https://linuxhint.com/search_replace_fish_shell/)

תהנו!
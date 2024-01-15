---
title:                "פענוח HTML"
html_title:           "PHP: פענוח HTML"
simple_title:         "פענוח HTML"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/parsing-html.md"
---

{{< edit_this_page >}}

## למה
פירוש המילה "הפנתרס" היא לתהליך של קריאה וניתוח של קוד HTML. המטרה העיקרית של הפנתרס היא ליצור מידע מבוסס מבנה לכתובת האינטרנט הנתון. כאשר משתמשים בפנתרס, ניתן להפיק מידע על רשאי השנית הנתונים ולבצע פעולות כיצד ניתן למצוא אותם.

## איך לעשות
בשפת PHP, כדי לאפשר הפנתרס לאתר את הקוד HTML, יש להשתמש בפעולת preg_match_all. לדוגמא, נתון מערך של כתובות אתרים, ניתן להשתמש בקוד הבא כדי למצוא את כל התגיות ה- `<a>` בכל כתובת:

```PHP
<?php
$links = array("https://www.example1.com", "https://www.example2.com", "https://www.example3.com");

// אתחול משתנה לתוך תצוגת משתנה לתוצאה
$results = "";

// ביצוע פעולת preg_match_all כדי למצוא את כל ה- <a> בכל כתובת
foreach ($links as $link) {
  preg_match_all('/<a href="(.+?)">/', file_get_contents($link), $matches);
  
  // הוספת תוצאות למשתנה תוצאות
  $results .= implode("\n", $matches[1]) . "\n";
}

// הדפסת התוצאות
echo $results;
?>
```

פלט:

```
https://www.google.com
https://www.facebook.com
https://www.twitter.com
```

כמו כן, ניתן גם להשתמש בפונקציה file_get_contents כדי לקרוא את קוד ה-HTML של אתר ולעבור עליו עם פקודות preg_match כדי למצוא מידע מפורט יותר.

## חקירה מעמיקה
ייתכן שתרצו לחקור יותר את מנגנון הפנתרס כדי ללמוד עוד על כיצד מתבצעת הניתוח של קוד HTML. בדוגמאות שהוצגו לעיל, השתמשנו בפונקציות שלדרך כלל משתמשים בהן בפרויקטי פיתוח אמיתיים. כמו כן, ניתן לחקור את כלי ה- DOMDocument שקיים בשפת PHP כדי לבצע פירוש של הפנתרסים ומידע
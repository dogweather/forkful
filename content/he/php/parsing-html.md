---
title:                "ניתוח HTML"
date:                  2024-01-20T15:33:09.939096-07:00
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
פענוח (Parsing) של HTML הוא תהליך שבו אנחנו מתרגמים ומנתחים את קוד ה-HTML לצורה שניתן לנהל אותה תכנותית. מתכנתים עושים זאת כדי לשנות, להוציא, או לאפיין מידע מתוך מסמכי HTML.

## איך לעשות:
```PHP
<?php
$doc = new DOMDocument();
@$doc->loadHTMLFile('path-to-your-html-file.html'); // התעלמות מאזהרות על HTML לא תקני
$xpath = new DOMXPath($doc);

// קבלת כל הקישורים
$links = $xpath->query('//a');
foreach ($links as $link) {
    echo $link->getAttribute('href') . PHP_EOL;
}
```

תוצאה:
```
http://example.com/page1
http://example.com/page2
...
```

## צלילה לעומק:
בעבר, פענוח HTML היה קרבה של רגקסים (regular expressions) וקוד מסורבל, אבל עם הזמן התפתחו כלים מתקדמים יותר כמו DOMDocument ו-DOMXPath ב-PHP, שמקלים על המשימה. האלטרנטיבות כוללות ספריות צד שלישי כמו Simple HTML DOM Parser ו-phpQuery, אבל DOMDocument הוא עמיד בזמן ומובנה ב-PHP. פרטי היישום כוללים את היכולת לעבוד עם XPath לשאילתות מורכבות, והמורכבות שבהתמודדות עם HTML לא תקני - שפתוח הקבצים עם "@" מטפל בכך על ידי התעלמות מהאזהרות.

## ראו גם:
- [PHP: DOMDocument](https://www.php.net/manual/en/class.domdocument.php) - המדריך הרשמי של PHP לעבודה עם DOMDocument.
- [PHP: DOMXPath](https://www.php.net/manual/en/class.domxpath.php) - המדריך הרשמי של PHP לשימוש ב-DOMXPath.
- [W3Schools XPath Tutorial](https://www.w3schools.com/xml/xpath_intro.asp) - מדריך ל-XPath, שישמש לניתוח וביצוע שאילתות במסמך HTML/XML.
- [Simple HTML DOM Parser](http://simplehtmldom.sourceforge.net/) - ספריית PHP לפרסור HTML שמציעה אינטרפייס פשוט יותר.

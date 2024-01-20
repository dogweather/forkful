---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/parsing-html.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 
ניתוח HTML הוא התהליך של קריאה וניתוח קוד HTML, במטרה להבין את מבנהו והמידע שהוא מכיל. תכנתים אותו כדי לאפשר ליישומים לאקום באופן יעיל עם מערכת HTML, מבלי להחמיא בנפילות שגיאות שיכולות להתרחש בכתיבה ידנית.

## איך לעשות:
```PHP
<?php
$doc = new DOMDocument();
libxml_use_internal_errors(TRUE); // פעל בשקט
$doc->loadHTML("<html><body>מה שלומך?<br></body></html>");
libxml_clear_errors(); // סיים בשקט
$xpath = new DOMXPath($doc);
$results = $xpath->query('//body');

if($results->length > 0){
  $body = $results->item(0);
  echo $body->nodeValue; // מה שלומך?
}
```
התוצאה של הקוד בהמשך הוא:
```PHP
מה שלומך?
```
## צלילה עמוקה
HTML Parsing הוא מהלך שהופעל שוב ושוב לאורך שנים. אף פעם לא היו אסטרטגיות חלופיות רבות. חלקם התחילו להשתמש בregex שעשתה את העבודה, אך הייתה מעט בעייתית. לכן, PHP מספק ספרייה DOMDocument שמכילה כלים מוכנים לשרת לביצועים יעילים.

יעילות היא מעלה חשובה במיוחד בבחינת XML ‏( שמהווה את בסיס הHTML ), חיסרונותיו של Regex הם ברורים ביותר. Regex נוטה להיות איטי, לא אמין וקשה לתחזוקה.

## ראה גם
* [HTML Parsing - w3schools](https://www.w3schools.com/php/php_ajax_rss_reader.asp)
* [DomDocument Class - PHP Documentation](https://www.php.net/manual/en/class.domdocument.php)
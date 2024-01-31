---
title:                "עבודה עם JSON"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם JSON"
simple_title:         "עבודה עם JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם JSON ב-PHP מאפשרת פרסון ואינקודינג של נתונים בפורמט טקסטואלי וחסכוני. תכניתנים עושים זאת לשיתוף נתונים באופן אפקטיבי בין שרתים ולקוחות.

## איך לעשות:
```PHP
<?php
// יצירת מערך
$myArray = array('שם' => 'דני', 'מקצוע' => 'מתכנת', 'גיל' => 30);

// אינקודינג ל-JSON
$json = json_encode($myArray, JSON_UNESCAPED_UNICODE);
echo $json;
// פלט: {"שם":"דני","מקצוע":"מתכנת","גיל":30}

// פרסון מ-JSON
$decodedArray = json_decode($json, true);
print_r($decodedArray);
// פלט: Array ( [שם] => דני [מקצוע] => מתכנת [גיל] => 30 )
?>
```

## צלילה לעומק
JSON (JavaScript Object Notation) הוא תסדיר נתונים המבוסס על טקסט שהומצא בתחילת שנות ה-2000. אלטרנטיבות נפוצות כוללות XML וYAML, אך JSON הפך למועדף בזכות פשטותו וקלות השימוש. PHP מציעה פונקציות עבור פעולות עם JSON כמו `json_encode` ו`json_decode`, המאפשרים גמישות בסדר ובתוספות כגון הדפסת Unicode בלי קוד המילוט.

## ראה גם
- [מדריך לפונקציות ה-JSON ב-PHP](https://www.php.net/manual/en/book.json.php)
- [בחירה בין JSON ל-XML ב-PHP](https://www.w3schools.com/js/js_json_xml.asp)
- [מבוא ל-JSON](https://www.json.org/json-en.html)

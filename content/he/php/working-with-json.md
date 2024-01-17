---
title:                "עבודה עם json"
html_title:           "PHP: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/working-with-json.md"
---

{{< edit_this_page >}}

מה ולמה?

 JSON הוא פורמט נתונים פופולרי בעולם התכנות המאפשר לנו לשמור ולשתף מידע בקלות. הרבה פעמים הוא משמש כפתרון לאחסון והעברת נתונים של אפליקציות ואתרי אינטרנט. תכניתנים נהנים מהשימוש ב-JSON כי הוא נוח לקריאה ולכתיבה, ואינו דורש שימוש ביניים נוספים כדי להתחיל לעבוד עםו.

כיצד?

קוד לדוגמה:

```php
<?php
// שמירת נתונים בפורמט JSON
$person = array(
  "name" => "John",
  "age" => 25,
  "hobbies" => array("drawing", "reading", "hiking")
);
$json_data = json_encode($person);

// קריאת נתונים מתוך קובץ JSON
$json_string = '{"name":"Jane","age":30,"hobbies":["cooking", "traveling", "coding"]}';
$json_data = json_decode($json_string);

// הדפסת הנתונים מתוך מערך
echo $json_data->name; // Jane
echo $json_data->hobbies[0]; // cooking
?>
```

פלט תוצאה:
```
Jane
Cooking
```

לחילוץ נתונים מתוך קובץ JSON, נשתמש בפונקציות json_encode ו-json_decode שקיימות בתוך שפת PHP. פונקציית json_encode ממירה מערך או אובייקט לפורמט JSON ופונקציית json_decode ממירה מחרוזת JSON למערך או אובייקט בפייתון.

עוד על סיפורו של JSON:

הפורמט JSON נמצא בשימוש כבר מזה כמה עשרות שנים, אך קיבל פופולריות גדולה עם צמיחת האינטרנט וצורך בפתרון שמאפשר לשתף נתונים מיעילים ברשת. עם התפתחותו של טכנולוגיות כמו AJAX ו-JSONP, הפורמט קיבל עוד יותר תהילה והופך לשימוש רחב יותר בתכנות.

אפשרויות אחרות:

הפורמט JSON אינו היחיד שקיים היום לאחסון ואיחול של נתונים בו זמנית. ישנם גם פורמטים נוספים כמו XML ו-YAML שמשמשים כאלטרנטיבות ל-JSON. אך, עם כל זאת, JSON מתאים במיוחד לשימוש בתכנות בגלל יחס הפשטות והגמישות שלו.

איך מיישמים את JSON ב-PHP?

להשתמש ב-JSON בפיתוח ב-PHP לא דורש התקנת כלי או ספריות נוספות. כל מה שצריך זה להשתמש בפונקציות json_encode ו-json_decode שכבר מובנות כלאים בשפת PHP. ניתן למצוא מידע מפורט יותר על השימוש ב-JSON באתר הרשמי של PHP.

רלוונטי גם לדעת:

למידע נוסף על פורמט JSON ושימוש בו בפיתוח, ניתן לעיין במסמכים ובמדריכים המצויינים באתרים של W3Schools ו-Codecademy.
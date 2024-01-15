---
title:                "עובדים עם yaml"
html_title:           "PHP: עובדים עם yaml"
simple_title:         "עובדים עם yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## למה
אם אנחנו רוצים לכתוב קוד עם מבנה מובנה וקל יותר לקריאה, אנחנו כנראה נתקל בפורמט הויתורית YAML. הילד החדש של מפתחי האינטרנט, YAML הוא קל להבנה ולתחזוקה והופך את העבודה עם קבצים מבולבלים לקלה יותר.

## איך לכתוב קוד עם YAML

תחילה, נצטרך להתקין את החבילה yaml באמצעות כדי לקודד ולפענח נתונים מתוך קבצים YAML. לאחר מכן, נצטרך לטעון את הקובץ שרוצים לעבוד אתו ולהמיר אותו למערך שאנחנו יכולים לעבוד איתו.

```PHP
// אימות החבילה yaml
extension_loaded('yaml') || die('install yaml extension first');

// טעינת הקובץ והמרתו למערך
$config = yaml_parse_file('config.yml');
```
עכשיו שיש לנו את המערך המכיל את כל הנתונים מהקובץ, אנחנו יכולים לקבל את הנתונים שאנחנו צריכים בצורה קלה יותר, כמו כן נוכל לעדכן אותם ולשמור אותם בזמן אמת.

```PHP
// אתחול משתנה שיכיל את שפת הממשק שהמשתמש בחר
$language = $config['user']['interface_language'];

// עדכון הקובץ ושמירה
$config['user']['interface_language'] = 'Hebrew';
file_put_contents('config.yml', yaml_emit($config));
```

## חפירה עמוקה
בנוסף לקריאה וכתיבה של נתונים מהקובץ YAML, ניתן גם לעשות זאת עם נתונים בתוך מחרוזות או כארגומנטים לפונקציות. לדוגמה:

```PHP
// נתונים בתוך מחרוזת YAML
$settings = yaml_parse('name: John Smith
age: 35');

// נתונים כארגומנט לפונקציה
function set_config(array $config) {
  // הגדרת משתנה גלובלי
  global $config;
  // שמירת הנתונים כמערך
  $config = $config;
}
set_config(yaml_parse_file('config.yml'));
```

וכמו בכל שפת תכנות אחרת, ח
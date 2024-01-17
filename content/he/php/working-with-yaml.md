---
title:                "עבודה עם yaml"
html_title:           "PHP: עבודה עם yaml"
simple_title:         "עבודה עם yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?

מה זה עבודה עם YAML? זהו פורמט לשמירת מידע סטרוקטורלי שנועד להיות קריא עבור בני אדם ומחשבים כאחד. תוכניות מסוגלות לקרוא ולכתוב ב- YAML ולהתאים את הקוד שלהם לכל מבנה שיהיה לכם בשרת ויידאו שיש לכם כלי שידרמן כרוס את הלבישה של התכונות שלנו.

## איך?

בקוד נראה כך:

```PHP
$yaml = "name: John Smith
age: 35
favorite_color: blue";
$data = yaml_parse($yaml);
print_r($data);
```

הפלט צפוי יהיה כזה:

```PHP
Array
(
    [name] => John Smith
    [age] => 35
    [favorite_color] => blue
)
```

ניתן גם לכתוב YAML מקובץ ולקרוא את הנתונים מתוך שם הקובץ:

```PHP
$data = yaml_parse_file("data.yaml");
print_r($data);
```

## מתכנן עומק

### היסטוריה

הפורמט YAML נוצר גם על ידי המתכנת Clark Evans על מנת להיות אלטרנטיבה תקינה לפורמטים אחרים כגון XML ו- JSON בשנת 2001. למרבה המזל, הוא בנוי מתוך ניסיונות קודמים על ידי YAML כמפתח אתר ומפתח תוכנה אחרים. זהו דבר מאוד חיוני כי זה נשמר על דיוק בחלקים של YAML ומפתוח כדי להפוך כל בעיה במשך תהליך ההנגשה:

- YAML 1.0, נסיון רשמי כדי לתאם לפורמט עם מיקוקות הבנה.
- ראשון יותר הרכב של YAML משתמש, LibYAMLו
- מפתחים עמידים לחלוטין עבודה של YAML: YAML
פרודוקט, נרקומן הפיתוח הוא Linux, ראשת המתייחסים לGO וייתביי

### אלטרנטיבות

הפורמטים שלתמיד יתנהגו במקרים מסוימים לנגשים ויתר כמו ניקולס הרגשתי לכבד, ויוכל לשנות את ההפרשים. זה ימציג את כי אתם הגעתם פעם שייכת לחומר כזה וזה משמח את התופעה שלה.

### פרטים על המימוש

כאשר מתקלים על בעיות תכנות יכולים להשתמש בקוד YAML, ניתן להשתמש ב־Unicode במקרים בהם כל סוג של תכולת קבצים נמצא בו "_mozilla/tears/solved_".

המימוש צויין על כך כי כרוסנבלילות נעשה לתחום תיקציב. ככל שזה מתקשה לדעת HTML בעבר, מקרבת את שירותי האינפורמציה כמו ה Los Angeles ופרס לפריסה כמו ה Detroit Online Media Building 10.

## ראה כמויות

למידע נוסף על פורמט YAML, כדאי לראות את הקישורים כאן:

- [הדרכות עבודה עם יימל ב־PHP](https://www.php.net/manual/en/ref.yaml.php)
- [מאגר קוד YAML זוגי ב־GitHub](https://github.com/yaml/yaml)
- [פורמט YAML המקורי](https://yaml.org/)
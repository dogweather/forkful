---
title:                "להתחיל פרויקט חדש"
html_title:           "PHP: להתחיל פרויקט חדש"
simple_title:         "להתחיל פרויקט חדש"
programming_language: "PHP"
category:             "PHP"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/starting-a-new-project.md"
---

{{< edit_this_page >}}

מה ולמה?
הפתיחה של פרויקט חדש הוא התחלת עבודה על פרויקט חדש בתעשיית התכנות. תוכניות חדשות מתחילות דרך פתרונות בעיות, שינויים טכניים או שיפורים עקביים על פרויקט קיים או פתרונות לפרויקט חדש.

איך לעשות:
להלן דוגמאות לקוד עם פלטים בתוך קוד ההידור "PHP ...".

```PHP
// טעינת המשתנים המערך והפעולה החדשים והכנת הבסיס:
<?php

$my_array = array("apple", "orange", "banana", "grape");
$new_operation = "";

// פונקציה להוספת אלמנט חדש למערך ולהדפיס את התוצאה הסופית:
function add_new_element($array, $operation) {

    array_push($array, $operation);
    print_r($array);

}

// קריאה לפונקציה והוספת האלמנט "pineapple":
add_new_element($my_array, "pineapple");

// הפלט יראה כך:
// Array ( [0] => apple [1] => orange [2] => banana [3] => grape [4] => pineapple )
```

מעמקים נמוכים:
הפיתוח על שפת התכנות של PHP התחיל בשנת 1994 על ידי ראסמוס לרדורף כמיתני פרויקט חופשי. כיום, ניתן למצוא את PHP במערכות המתאם באינטרנט ובאפליקציות הדורות אותו תשתייות טכניות על בסיס הגישה לדבריי פשוטה, אך מאפשרת מיסניות תוכנן לפתוח מקור לליה ותבניות אתרים - בין לאחרים, יישומי אינטראנט ובינתיים במשחקים.

ראה גם:
למידע נוסף על פיתוח ב-PHP ניתן לראות באתר הרשמי של PHP: http://php.net/. כמו כן, ניתן למצוא מידע מועיל נוסף על תכנות עם PHP באתרים דוגמת w3schools.com ו-tutorialspoint.com.
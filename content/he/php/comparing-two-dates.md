---
title:                "PHP: השוואת שתי תאריכים"
simple_title:         "השוואת שתי תאריכים"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה
מדוע יש צורך לטפל בהשוואת שני תאריכים? לעיתים קרובות נחזה לטפל בתאריכים כחלק מתהליך התכנות. היכולת להשוות שני תאריכים מאפשרת לנו לבדוק האם תאריך מסוים נמצא לפני או אחרי תאריך אחר ולפעול בהתאם. במאמר זה נעסוק בדרכים שונות להשוות שני תאריכים בשפת PHP.

## כיצד לעשות זאת
נוכל להשתמש בפונקציה המובנית של PHP strtotime() כדי להמיר תאריך טקסטואלי לתאריך Unix timestamp ולאחר מכן להשתמש באופרטורי השוואה כדי להשוות בין שני התאריכים. לדוגמה:

```PHP
$first_date = "2021-01-01";
$second_date = "2021-01-15";

if (strtotime($first_date) < strtotime($second_date)) {
    echo "$first_date נמצא לפני $second_date";
} elseif (strtotime($first_date) > strtotime($second_date)) {
    echo "$first_date נמצא אחרי $second_date";
} else {
    echo "שני התאריכים זהים";
}

// פלט: "2021-01-01 נמצא לפני 2021-01-15"
```

נוכל גם להשתמש בפונקציה המובנית של PHP strtotime() כדי להמיר שני תאריכים לתאריך Unix timestamp ולאחר מכן להשתמש בפונקציה date() כדי להציג את התאריך לפי הפורמט הרצוי. לדוגמה:

```PHP
$first_date = "2021-01-01";
$second_date = "2021-01-15";

$first_timestamp = strtotime($first_date);
$second_timestamp = strtotime($second_date);

echo date("d/m/Y", $first_timestamp) . " נמצא לפני " . date("d/m/Y", $second_timestamp);

// פלט: "01/01/2021 נמצא לפני 15/01/2021"
```

## כיול עמוק
כאשר אנו משווים שני תאריכים, חשוב לקחת בחשבון את הצורך בכיול עמוק. כלומר, לבדוק את התאריך המלא ולא רק את היום או השנה. לדוגמה, שני תאריכים יכולים להיות שווים ביום נתון, אך להיות שונים בזמן.

נוכל לכוון כי
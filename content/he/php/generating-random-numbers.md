---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת מספרים אקראיים ב-PHP היא למעשה מנגנון שמפיק מספר שונה בכל פעם שהוא נקרא. זה מאוד שימושי כאשר אתה צריך ליצור טוקן אבטחה, לבחור באקראיות איבר מתוך מערך, או כאשר אתה מממש משחקים.

## איך:
קוד PHP ליצירת מספר אקראי יכול להראות כך:
```PHP
<?php
$randNum = rand(1, 100); // Generate a random integer between 1 and 100 
echo $randNum;  // Outputs the random number 
?>
```
במקרה הזה, יצרנו מספר אקראי בין 1 ל-100 ואז הדפסנו אותו.

## היכנס עמוק יותר:
אף על פי שהפונקציה `rand()` היא זו שאנחנו שימשנו הכי הרבה בעבר, PHP 7 הציגה שני פונקציות חדשות ליצירת מספרים אקראיים: `random_int()` ו- `random_bytes()`. תחזית הרנדומליזציה של שני אלה מנותחת בחומרה יותר, ולכן הן מועדפות כאשר האינטרס הוא בטוקנים אבטחה.

## ראה גם:
1. הסברים נוספים על `random_int()`: [לינק](https://www.php.net/manual/en/function.random-int.php)
2. מידע נוסף על `random_bytes()`: [לינק](https://www.php.net/manual/en/function.random-bytes.php)
3. מאמר שלנו החופש על נושא Tokens ב-PHP.
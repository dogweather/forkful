---
title:                "כתיבת מבחנים"
html_title:           "PHP: כתיבת מבחנים"
simple_title:         "כתיבת מבחנים"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/writing-tests.md"
---

{{< edit_this_page >}}

## מה זה ולמה?

כתיבת בדיקות היא תהליך שאנחנו, כמתכנתים, מבצעים כדי לוודא שהקוד שלנו עובד כפי שצריך. בדיקות ממומנות מסייעות לנו לזהות שגיאות ותקלות בקוד שלנו ולתקן אותן בשלב מוקדם יותר, כך שהקוד שאנחנו מפרסמים יהיה במצב מושלם יותר.

## איך לעשות:

```PHP
<?php
// כתיבת פונקציות לבדיקות
function add($num1, $num2) {
  return $num1 + $num2;
}

function test_add() {
  if (add(2, 2) === 4) { // בדיקה אם התוצאה תואמת למצופה
    echo "הבדיקה עברה בהצלחה";
  } else {
    echo "הבדיקה נכשלה";
  }
}

test_add(); // הפעלת הבדיקה
?>
```

כפי שאתם יכולים לראות בדוגמא הזו, אנו משתמשים בפונקציות כדי לבדוק את התצוגה של קודנו. הפונקציות הללו בדרך כלל מתבצעות על ידי עוד פונקציות של בדיקה וסידור אותם בקוד שלנו.

## חפירה עמוקה:

בעבר, היה לקבוצת תוכנית מיוחדת בשם "QA" (איכות התוכנה) שהייתה מפעילה את הבדיקות שלנו על ידי ניסיון ומתן משותפות. ביום זה, יש לנו מספר כלי אוטומטיים שעושים את העבודה עבורנו, כך שהמוצרים שלנו יכולים להיות באיכות טובה יותר בזמן יותר קצר.

אם אתם מתקשים לכתוב סיפורי בדיקה, שיטות אחרות כמו Test-Driven Development ו- Behavior-Driven Development יכולות לעזור לכם. הן עזרו לכם לרדת לפרטים יותר מדי המימוש שלנו, ולסייע לנו להגיע לפתרון המתאים יותר.

אם אתם רוצים לדעת יותר על כתיבת בדיקות ב-PHP, יש לנו לכם כמה קישורים שיעזרו לכם להתחיל:

- [PHPUnit](https://phpunit.de/): כלי ייחודי לבדיקת PHP
- [Official PHP Documentation](https://www.php.net/manual/en/book.php://www.php.net/manual/en/book.php): מידע רשמי על בדיקות בפי-פי-פי
- [Tuts+ PHP Testing series](https://code.tutsplus.com/series/the-buzz-on-php-testing--cms-827): סדרת המדריכים החמים כדי ללמוד כיצד לבדוק את קוד שלנו בפי-פי-פי

## ראו גם:

תקווה שתהנו מכתיבת בדיקות ותטפלו בקוד שלכם בצורה עדינה יותר. ישנן כמה דרכים שונות לכתוב בדיקות ויש לנו מאמרי נושא נושאים נוספים שיעזרו לכם להתחיל.

- [Test Driven Development for Beginners](https://www.codebyamir.com/blog/test-driven-development-for-beginners)
- [What is BDD? (Behavior Driven Development)](https://www.codebyamir.com/blog/what-is-bdd)
- [Jest: PHP Testing Made Easy](https://www.codebyamir.com/blog/jest-php-testing-made-easy)
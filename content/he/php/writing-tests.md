---
title:    "PHP: כתיבת בדיקות"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/php/writing-tests.md"
---

{{< edit_this_page >}}

## למה

בהתכנסות פיתוח התוכנה כיום, כתיבת טסטים נעשה בדרך כלל חלק לא נפרד מכתיבת הקוד. כתיבת טסטים מאפשרת לנו לוודא כי הקוד שכתבנו עובד כפי שצוין ואינו גורם לבאגים. זה גם יכול לעזור לנו לשמור על איכות הקוד ולקפוץ על באגים במהירות יותר.

## איך לעשות זאת

בכדי לכתוב טסטים ב-PHP, יש להשתמש ב-framework כמו PHPUnit או PHPSpec. באמצעות ה-framework הזה, ניתן לכתוב טסטים בצורה מאורגנת וקלה לניהול. לדוגמה, ניתן לכתוב טסטים שמאמתים את הפרסומים במערכת הבלוג שלנו:

```PHP
<?php
use PHPUnit\Framework\TestCase;
class BlogPostTest extends TestCase
{
    public function testPublishPost()
    {
        $post = new BlogPost();
        $post->setTitle('כותרת פוסט');
        $post->setBody('תוכן פוסט');
        $post->setAuthor('יובל כהן');
        $post->publish();
        $this->assertTrue($post->isPublished());
    }
}
```

כאן, אנחנו מגדירים טסט בשם "testPublishPost" שבודק אם לאחר פרסום הפוסט, התכונה "פרסומי" הבלוג נקבעה ל- "true". כך ניתן לוודא שהפוסט נמצא במצב מתאים לפרסום ואין בעיות בקוד.

## חפירת מעמקים

כתיבת טסטים מביאה הרבה יתרונות לפיתוח התוכנה שלנו. הם מאפשרים לנו לאמת את הנתונים ואת התוצאות של הקוד, לזהות הפרטים העמוקים שלו ולקודד את התכונות שלו בצורה קלה יותר לניהול בעתיד. החשיבה על טסטים כחלק חשוב מתהליך הפיתוח יכולה לעזור לנו לכתוב קוד יותר ברור ומתודלק.

## ראה גם

- [PHPUnit](https://phpunit.de/)
- [PHPSpec](https://www.phpspec.net/)
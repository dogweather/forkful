---
title:                "כתיבת מבחנים"
html_title:           "Ruby: כתיבת מבחנים"
simple_title:         "כתיבת מבחנים"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות היא תהליך שבו מתבצעת בדיקה ווודאות של קוד תוכנה על מנת לוודא שהוא עובד כפי שצריך ואין בו באגים. תהליך זה חשוב מאוד לכל מתכנת, הייתו זה מתחילים או מקצועיים, כיוון שהוא מסייע למנוע באגים ולשפר את איכות הקוד.

## איך לעשות?
```Ruby
def add_numbers(a, b)
  return a + b
end

puts add_numbers(3,5)
```
Output:
```
8
```

## שקיפות לעומק
ניתן למצוא את התורשות ההיסטורית של בדיקות תוכנה חזקה עד מה גם בתקופת ימי המחשבים הראשונים. כיום, ישנן אלטרנטיבות רבות לכתיבת בדיקות שבהן מתבצעות בדיקות אוטומטיות שנותנות תוצאות מדויקות יותר. בכל זאת, הכתיבה של בדיקות ידניות עדיין נחשבת לפנטסטית על מנת לבדוק ולוודא כי כל תיכנות הוא תקין.

## ראו גם
למידע נוסף על כתיבת בדיקות תוכנה ב-Ruby, ניתן לעיין במקורות הבאים:
- [בסיס מידע על כתיבת בדיקות תוכנה עם Ruby] (https://blog.testdouble.com/posts/2019-11-04-ruby-testing-101.html)
- [מדריך של רובי ליסודות הבדיקות] (https://ruby-doc.org/core-2.7.1/MiniTest.html)
- [אמרזון ועלויות של מעבדת בבדיקת איכות מוצרים] (https://aws.amazon.com/blogs/devops/testing-in-production-in-amazon-ecs-using-aws-codepipeline-aws-fargate-and-minimizing-impact-with-amazon-cloudfrontStub)
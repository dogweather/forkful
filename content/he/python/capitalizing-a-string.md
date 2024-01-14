---
title:                "Python: שיפור מחרוזת בתוכנות"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

אילו: מדוע לדבר על לעורר סדרת הטקסט.

C++- תוכנית הדוגמה Python הנושא הזה מטריד רבים מבעלי שפות תכנות אחרות, כדי להבין אותו למם קל מאוד. במקור, השימוש בפיתחי(ת) התוכנה,

`Python
string = "hello world"
print(string.capitalize())  #Output: Hello world
`
## איך לעשות את זה

האופן הכי פשוט לעורר סדרות טקסט בפייתון הוא על ידי שימוש בשיטת "capitalize ()". פשוט כתוב את המחרוזת שברצונך לעורר ולקרוא את הפעולה "capitalize ()". לדוגמה:

```Python
string = "hello world"
print(string.capitalize())  #Output: Hello world
```

ניתן גם לעורר את המחרוזת הזו במצב תחילית, על ידי שימוש בפיתחי(ת) שפת תכנות אחרים או לעבד את הנתון בצורה יעילה יותר.

```Python
sentence = "i love python"
print(sentence.capitalize())  #Output: I love python
```

כדי לעורר סדרות טקסט תחת שורת, עליך פשוט לכתוב את הפקודה "capitalize()" תחת המחרוזת הרלוונטית:

```Python
string = "this is a sentence."
print(string.capitalize())  #Output: This is a sentence.
```

## טיול עמוק

פיתחי(ת) שפת תכנות פייתון בכלל, ומי שעובד בניהם מיודע שלטיפול בסדרות טקסט הוא חלק לא נסבל ביותר של תחביר שפת תכנות. כאשר מדובר על לעבוד עם סדרות טקסט גדולות ומורכבות, חשוב להתחיל כבר בשלב מוקדם עם יכולות כמו עיצוב וטפספוס של הסדרות. כתבו את הפקודה capitalize כדי להיותוחלות כהרץ את התהליך באופן מסודר ויעיל.
פיתחי(ת) יכולים גם להשתמש בפיתח TutCaps שלהם כדי למצות באלפיט כדי התגובה פייתון. לעלא מתורה לעבוד עם סדרות טקס
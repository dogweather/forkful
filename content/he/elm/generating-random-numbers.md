---
title:                "Elm: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## למה

בעולם התכנות, יצירת מספרים אקראיים היא כלי חשוב לכמה מטרות. תחת נושא זה אני רוצה לדבר על כמה היבטים שליליים שמתגלים באפשרות זו.

## איך לבצע

הנה דוגמא פשוטה של כיצד ניתן ליצור מספר אקראי באמצעות שפת אלם:

```elm
import Random

Random.generate Random.int 1 10
```

פלט יוצא יהיה מספר אקראי בין 1 ל-10. לא נראה קשה, נכון?

ניתן לכוון את הקוד כדי ליצור מספרים אקראיים בין מספרים מסוימים, כמו גם לשלוט בכיוון שלהם. בדוגמא הזו נבצע זאת באמצעות פונקציות `Random.float` ו-`Random.step`:

```elm
import Random

Random.generate (Random.step (0.5, 1)) (Random.float 0 1)
```

כך, הפלט יהיה מספר אקראי בין 0 ל-1, אבל רק בצעדים של 0.5 עד 1.

אם תרצו, תמיד ניתן ליצור יותר ממספר אחד באותו הפעלת הפונקציה:

```elm
Random.generate twoRandomNumbers (Random.int 1 10, Random.float 0 1)

twoRandomNumbers : Int -> Float -> (Int, Float)
twoRandomNumbers =
    (,)
```

פעולה זו תציג את שני המספרים האקראיים ביחד כנ對elementsנספרים לפי הסדר.

## יכולת למעמער

יצירת מספרים אקראיים מגיעה עם אפשרויות אין סופיות. ניתן להשתמש בפונקציות נוספות כמו `Random.shuffle` ו-`Random.pair` כדי ליצור תוכניות סוללדוי ולהוסיף יכולות מתקדמות לאפליקציות שלנו.

אחת האפשרויות המעניינות היא ליצור נקודות אקראיות על מפה גיאוגרפית. בדוגמא הזו ניצור שתי קורודינטות אקראיות של נקודות על פני הארץ:

```elm
import Random
import Geocoder

Random
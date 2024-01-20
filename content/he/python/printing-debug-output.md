---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?

דיבאג (Debug) הוא הדרך שבה אנו מתוודעים למה מריצ התוכנית שלנו מתנהג כפי שהוא מתנהג. אנחנו משתמשים בזה כדי להבין את החוליות שממנו מורכבת התוכנית שלנו, ולאמת שהן מתנהגות כמצופה.

## איך עושים:

דוגמא בסיסית של הדפסת הודעת דיבאג בפייתון:

```Python
def is_prime(n):
    print("Debug: Checking if", n, "is prime.")
    for i in range(2, n):
        if n % i == 0:
            print("Debug: Found a factor:", i)
            return False
    return True

print(is_prime(7))
```

הפלט של הדוגמא הזאת יהיה:

```
Debug: Checking if 7 is prime.
True
```
  
## צלילה עמוקה:

ראשית, קצת היסטוריה. הדפסת הודעות דיבאג היא שיטה עתיקה שמשמשת מתכנתים ברחבי העולם. לאחר מכן בשפות כמו פייתון, ישנן כלים מובנים לדיבאג, כמו המודול `logging`.

דיבאג מתכותר כאחד מהדרכים הכי מהירות והכי פשוטות ללמוד על בעיות בקוד שלך. זה לא הכלי היעיל ביותר, אבל הוא מספיק אם אתה בספק למה תוכנית מסוימת לא מתנהגת כמצופה.

## ראו גם:

1. מדריך לדיבאג בפייתון: https://realpython.com/python-debugging-pdb/
2. מודול ה-logging של פייתון: https://docs.python.org/3/library/logging.html
3. כלים נוספים לדיבאג: https://pythontips.com/2013/07/30/20-python-libraries-you-cant-live-without/
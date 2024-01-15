---
title:                "כתיבת בדיקות"
html_title:           "Python: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/writing-tests.md"
---

{{< edit_this_page >}}

## למה

במהלך כתיבת קוד בפייתון, חשוב לבדוק את הקוד שלנו על מנת לוודא שהוא עובד כצפוי ולזהות בעיות מבולגנות במקורן. בכדי לעשות זאת, אנו משתמשים בבדיקות, שהן חלק בלתי נפרד מפרקטיקת כתיבת קוד בפייתון.

## איך ל

לדוגמה, נכתוב פונקציה פשוטה שמכפילה שני מספרים ומחזירה את התוצאה:

```Python
def multiply(x, y):
    return x * y
    
result = multiply(3, 4)
print(result)
```

הפלט שלנו יהיה 12, כפי שציפינו. אם נרצה לקבל אישור נוסף שהפונקציה עובדת כצפוי, נוסיף בדיקה עם הערת תיעוד לפונקציה:

```Python
def multiply(x, y):
    """
    Multiplys two numbers and returns the result.
    """
    assert multiply(3, 4) == 12
    return x * y
    
result = multiply(3, 4)
print(result)
```

השימוש ב- assert מאשר את הערך שאנו מצפים לקבל, ואם התוצאה לא תתאים לציפייה, הבדיקה תכניס שגיאת AssertionError. ניתן להשתמש גם בספריית הבדיקות המובנית בפייתון - unittest, כדי לכתוב בדיקות מפורטות יותר וכדי לארגן את הקוד לפי תתי-בדיקות.

## עיון מעמיק

כתיבת בדיקות רבות עוזרת לנו לזהות בזמן את הבעיות הקודמות ולהימנע מהתנהגות לא צפויה בקוד. חשוב לכתוב בדיקות לכל רמות הקוד - מהפונקציות הקטנות לתוכניות הגדולות - על מנת לוודא שהכל עובד כצפוי. בנוסף, כתיבת בדיקות מסייעת בשמירת על קוד נקי וקל יותר לתחזוקה.

## ראו גם

- [ספריית הבדיקות המובנית בפייתון](https://docs.python.org/3/library/unittest.html)
- [מדריך ל
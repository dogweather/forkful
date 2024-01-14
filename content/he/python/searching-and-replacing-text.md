---
title:                "Python: חיפוש והחלפת טקסט"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה?

למה לפתח בקוד Python? אחד המשימות הנפוצות בתכנות הוא החיפוש וההחלפה של טקסט בקוד. בעזרת פייתון, ניתן לעשות זאת בקלות ובמהירות, ולכן הוא מיועד למתכנתים שמחפשים כלי עוצמתי לעבודתם.

## כיצד לעשות זאת?

הראה לנו כיצד ניתן לחפש ולהחליף טקסט בקוד Python:

```python
phrase = "שלום לכולם, אני מלמד Python!"
new_phrase = phrase.replace("Python", "פייתון")
print(new_phrase)
```

תוצאה:

```
שלום לכולם, אני מלמד פייתון!
```

בדוגמה זו, אנו משתמשים בפונקציה המתאימה בשם "replace" שתחליף את המחרוזת "Python" במחרוזת "פייתון". כך ניתן לשנות טקסט באופן דינמי בתוך הקוד שלנו.

## העומק

בנוסף לפונקציה "replace", ישנן עוד מספר פונקציות שיכולות ללכלך ולטפל בטקסט בקוד Python. כמה דוגמאות לכך יכולות להיות:

- פונקציה "findall" שתחפש את כל המופעים של מחרוזת מסוימת בתוך טקסט.
- פונקציה "strip" שתסיר רווחים ותווים חסרים מסביב למחרוזת.
- פונקציה "capitalize" שתפרט את האות הראשונה של מחרוזת לאות גדולה, ואת שאר המחרוזת לאותיות קטנות.

אלו רק כמה דוגמאות מתוך מגוון הפונקציות שקיימות לניהול ולעיבוד טקסט בקוד Python. כדאי ללמוד אותן כדי להשתמש בהן במהירות וביעילות.

## ראו גם

- [Python מדריך רשמי](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [איך להתמודד עם טקסטים בקוד Python](https://realpython.com/string-manipulation-python/)
---
title:    "Python: חיפוש והחלפת טקסט"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מדוע

בעת התכנות בפייתון, ישנם רבים מצבים שבהם יהיה צורך לחפש ולהחליף טקסט בתוך קוד. למשל, ייתכן שיהיו שגיאות כתיב בחלק מהמשתנים או ייתכן שאנחנו רוצים לשנות משהו בטקסט המוצג למשתמש באפליקציה. בעזרת חיפוש והחלפה, אנו יכולים לבצע כך בקלות ובמהירות רבה.

## איך לעשות זאת

לפניכם דוגמאות לשימוש בפונקציות חיפוש והחלפה עם פייתון. שימו לב שהפעלת חיפוש והחלפה תבוצע מתוך תיקית העבודה הנוכחית.

```python
# חיפוש והחלפה בכל הטקסט שמתאים לתבנית מסוימת
import re

text = "Hello, my name is John. I am from Israel."
new_text = re.sub(r"John", "David", text)

print(new_text)
# Output: Hello, my name is David. I am from Israel.

# חיפוש והחלפה בכל הקבצים שסיומים ב-ext
import os, re

file_list = os.listdir()
ext = input("Extension: ")

for file in file_list:
    matched = re.search(rf"{ext}$", file)

    if matched:
        new_file = re.sub(r"old_text", "new_text", file)
        os.rename(file, new_file)

print("Files renamed successfully!")
```

## עומק נוסף

ניתן להשתמש בתבניות מתקדמות יותר לחיפוש והחלפה, כגון שימוש בפונקציות כמו split ו- join, או להשתמש בתוסף כמו re.Match המאפשר לגשת למידע נוסף על התאים המתאימים בטקסט.

## ראו גם
- [מדריך לתוספי פייתון לחיפוש והחלפה](https://realpython.com/regex-python/)
- [מדריך לביטויים רגולריים בפייתון](https://www.analyticsvidhya.com/blog/2019/03/beginner-guide-regex-python/)
- [מדריך לתרגול חיפוש והחלפה בפייתון](https://www.w3schools.com/python/python_regex.asp)
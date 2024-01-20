---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?

קריאת קובץ טקסט בפייתון היא התהליך שבו משתמשים בפקודות בקוד כדי לקרוא תוכן שנמצא בקובץ. מתכנתי פייתון עושים זאת כדי לטפל בנתונים, לנתח פלטים ולשם גיבוי ושחזור.

## איך לעשות את זה:

```Python
# פתח קובץ
text_file = open('example.txt', 'r')

# קרא קובץ
print(text_file.read())

# סגור קובץ
text_file.close()
```

פלט:

```
זהו דוגמא לטקסט שנקרא מקובץ.
```

## הצצה לעמוק ביותר

בעבר, בשפות תכנות אחרות, היה צורך לכתוב קוד מורכב יותר כדי לקרוא קבצים. פייתון מאפשרת לנו לעשות את זה בקלות יותר. יתר על כן, באופן אלטרנטיבי, ניתן לשימוש ב-with statement במקום לסגור את הקובץ באופן ידני. בנוסף, אם הקובץ גדול מאוד, עלול להיות טוב יותר לקרוא את הקובץ בצורה שונה כדי לחסוך בזיכרון.

```Python
with open('example.txt', 'r') as text_file:
    for line in text_file:
        print(line)
```

## ראה גם

[מדריך רשמי על טיפול קבצים בפייתון](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)

[אתר StackOverflow עם שאלות ותשובות על הנושא](https://stackoverflow.com/questions/tagged/python+file-io)

[מאמר מעמיק המסביר על טיפול בקבצים בפייתון](https://realpython.com/read-write-files-python/)
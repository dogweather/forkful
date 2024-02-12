---
title:                "הסרת מרכאות ממחרוזת"
aliases:
- /he/python/removing-quotes-from-a-string/
date:                  2024-01-26T03:43:35.754683-07:00
model:                 gpt-4-0125-preview
simple_title:         "הסרת מרכאות ממחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
הסרת מרכאות ממחרוזת פירושה לרוב הסרת סימני ציטוט מיותרים - כפולים (") או יחידים ('). מתכנתים עושים זאת כדי לנקות קלט או כאשר אין צורך בציטוטים לעיבוד נוסף - לדוגמה, בעת שמירת טקסט למסד נתונים או הכנתו לתצוגה.

## איך לעשות זאת:
פייתון מציעה מספר דרכים להיפטר ממרכאות לא רצויות ממחרוזות. בואו נעבור דרך כמה דוגמאות:

```Python
# דוגמה 1: שימוש ב-str.replace() להסרת כל המופעים של מרכאות
quote_str = '"Python is awesome!" - Some programmer'
no_quotes = quote_str.replace('"', '')
print(no_quotes)  # פלט: Python is awesome! - Some programmer

# דוגמה 2: שימוש ב-str.strip() להסרת מרכאות רק מהקצוות
quote_str = "'Python is awesome!'"
no_end_quotes = quote_str.strip("'")
print(no_end_quotes)  # פלט: Python is awesome!

# דוגמה 3: טיפול גם במרכאות יחידות וגם בכפולות
quote_str = '"Python is \'awesome\'!"'
no_quotes = quote_str.replace('"', '').replace("'", "")
print(no_quotes)  # פלט: Python is awesome!
```

## צלילה עמוקה:
המנהג של הסרת מרכאות הוא כמו עתיק יומין של תכנות מחשבים. במקור, היה זה עניין פשוט של ניקוי נתונים. עם התפתחות המערכות והתחלת האינטראקציה בין שכבות שונות - כמו ממשק משתמש, שרת, ומסד נתונים - ניקוי מחרוזות הפך להכרחי כדי למנוע שגיאות או בעיות אבטחה. לדוגמה, זריקות SQL ניתנות למניעה על ידי הסרת מרכאות או הימנעות מהם בקלטי משתמש לפני הכנסת הנתונים למסד הנתונים.

חלק מהחלופות לשיטות שהוצגו לעיל כוללות ביטויים רגולריים, אשר יכולים להיות יתר על המידה להסרת ציטוטים פשוטה אך חזקים להתאמת תבניות מורכבות. לדוגמה, `re.sub(r"[\"']", "", quote_str)` תחליף את כל המופעים של מרכאות יחידות או כפולות במחרוזת ריקה.

בעת ביצוע הסרת מרכאות, זכרו שההקשר חשוב. לעיתים יש צורך לשמר מרכאות בתוך מחרוזת אך להסיר אותן מהקצוות, ולכן `strip()`, `rstrip()` או `lstrip()` הם חברים שלכם. מצד שני, אם אתם צריכים להסיר את כל המרכאות או לטפל במרכאות מקודדות כמו `&quot;`, כנראה שתפנו אל `replace()`.

## ראו גם:
- [תיעוד מחרוזות של פייתון](https://docs.python.org/3/library/string.html)
- [ביטויים רגולריים בפייתון (מודול re)](https://docs.python.org/3/library/re.html)
- [מדריך OWASP למניעת זריקת SQL](https://owasp.org/www-community/attacks/SQL_Injection)

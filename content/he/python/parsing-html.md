---
title:                "פיענוח HTML"
html_title:           "Python: פיענוח HTML"
simple_title:         "פיענוח HTML"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/parsing-html.md"
---

{{< edit_this_page >}}

## מה זה ולמה?

פרסום HTML הוא תהליך של קריאה וניתוח של קוד HTML על מנת לחלץ מידע מגובה איברים בעמוד האינטרנט. תהליך זה חשוב למתכנתים בכדי לאפשר עיבוד והצגת מידע בצורה חכמה.

## איך לעשות:

כדי לקרוא קוד HTML בעזרת פייתון, ניתן להשתמש בספריית html.parser שהיא חלק מהספרייה המובנית של פייתון. הקוד הבא מדגים איך להשתמש בספריית זו כדי לפרסם את תוכן אלמנט מסוים בעמוד אינטרנט:

```Python
from html.parser import HTMLParser
class MyHTMLParser(HTMLParser):
    def handle_starttag(self, tag, attrs):
        if tag == "p":
            print("Found a paragraph!")
    def handle_data(self, data):
        print(data)

parser = MyHTMLParser()
example_html = "<html><p>My first paragraph</p><p>My second paragraph</p></html>"
parser.feed(example_html)
```
Output:
```
Found a paragraph!
My first paragraph
Found a paragraph!
My second paragraph
```

## כיול עמוק:

פרסום HTML נוצר כדי לאפשר למתכנתים להוציא תוכן מהאינטרנט ולעבד אותו בצורה נוחה ויעילה. ישנן גם אפשרויות אחרות כמו ספריות חיצוניות שניתן להשתמש בהן לפרסום HTML, כגון BeautifulSoup. לפצת ספרייה מובנית בפייתון הוא הבחירה האופטימלית כדי למנוע התחברות נוספת.

ספריית html.parser משתמשת בתיקן פייתון תוך כדי עיבוד קוד HTML, כאשר תיקן זה מאפשר לכל הסמלים להופיע באופן אדיקטבי ללא תשובה למשתנים המסורבים. פרסום HTML גם יכול לשמש ככלי אבא מחזק פייתון.

## ראה גם:

למידע נוסף על פרסום HTML ואיך להשתמש בו בפייתון, ניתן למצוא מידע באתר הרשמי של פייתון ובמסמך המפורסם על ספריית html.parser.

אתר הרשמי של פייתון: https://www.python.org/

מסמך html.parser: https://docs.python.org/3/library/html.parser.html
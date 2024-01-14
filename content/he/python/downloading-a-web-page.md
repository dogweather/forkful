---
title:                "Python: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# מדוע

הורדת עמוד רשת יכולה להיות כלי עזה עבור מתכנתים שונים. היא מאפשרת לנו לגשת למידע מקוון ולעבוד איתו בפיתוחנו. לדוגמה, ניתן להשתמש בכיבוד על ה-HTML, לנתח דפי אינטרנט ולייצר אתרי כבוד יוצאים על ידי השתמשות בכלי זה. אין שום ספק שהייונכםהווה את חשבון.

# לב "כיצ עמוד רשת יא (web page) באמת (Python)

עם למעלה שנכתוב בתנאות קוד כדי להורדה עמוד רשת עם Python. למשל, נלקח את הדף הבא: "https://www.python.org/".

```python
# Import the requests library
import requests

# Use the get() method to access the web page
page = requests.get("https://www.python.org/")

# Print the HTML content of the page
print(page.text)
```

Output:

```python
<!doctype html>

<html class="no-js" lang="en" dir="ltr">
<head>
	<meta charset="utf-8">
	<meta http-equiv="X-UA-Compatible" content="IE=edge">
	<meta name="viewport" content="width=device-width, initial-scale=1">
	<link rel="stylesheet" href="/static/stylesheets/style.c1ffeba4013f.css" type="text/css" title="default" />
	<link rel="stylesheet" href="/static/stylesheets/mq.c1ffeba4013f.css" type="text/css" />
	<!--[if lte IE 8]>
	<link rel="stylesheet" href="/static/stylesheets/no-mq.2b1ca09b6e4b.css" type="text/css" />
	<![endif]-->
	<link rel="alternate" type="application/rss+xml" title="Python Enhancement Proposals" href="https://www.python.org/dev/peps/peps.rss/">
	<title>Welcome to Python.org</title>
	...

	...
</html>
```

כפי שאתםויודעים, לא תמיד הכיבוד יוצא כמו שמתרגלים לו. אם אתם יכולים להילחם אותם ולנתחם אותם, אתם תשגיחוווטוושםהיהציבור

# העמימהיונשולום עמוד רשתי

כעת, נעמור יותר בעומק על איך לנתח עמודי אינטרנט בעזרת Python. בכדי לגשת לתוכן של עמוד רשת, ניתן להשתמש במכשיר הכיבוד של רכישה. כמו כן, ניתן להשתמש בכלי ניתוח בכיבוד או לכתוב עמודי מכשיר בכדי לעבוד עם התוכן. הנה דוגמת קוד שניתן להשת
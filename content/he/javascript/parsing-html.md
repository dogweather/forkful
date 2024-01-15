---
title:                "פיענוח html"
html_title:           "Javascript: פיענוח html"
simple_title:         "פיענוח html"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/parsing-html.md"
---

{{< edit_this_page >}}

# למה

למה אתה צריך לקרוא קוד HTML? איך אתה יכול ליצור ולערוך אתרים מעניינים ומצטיינים באמצעות לשוניות כגון גרור ושחרר וכמו הנספחים הרבים שאתה יכול להוסיף או למחוק באתר שלך. ועוד מספר תכונות שמציינים את הדרך בה התכניות הפכו להיות מתקדמות יותר, כגון גרסאות קוד HTML, סוגי מיחשוב שונים כמו וידלינג ואפשרויות שימושיות עם קודים לאינטגרציה של דפים או אלמנטים.

למה נדרשים להמחיש דפים?

לעולם לא ניתן לערוך תוך כדי ניווט אינטואיטיבי ויישום רב כללי של קוד לכניסה של המלאכה. למעשה, חלון הפתיחה כולו הוא לא אליצציה מקרהית ממה שה power heeft מאחר וקוד הדפדפן שלך משתמשים.

# איך לעשות

### כתיבת קוד HTML שכאן

```Javascript
<!DOCTYPE html>
<html>
<head>
	<title>הדגמת סוגי מיספור שמכילים שמות סוגי מיספור</title>
	<style>
		body{
			font-family: Arial, sans-serif;
		}
	</style>
</head>
<body>
	<h1>מונה את החודרים!!</h1>
	<ul>
		<li>רעדן</li>
		<li>אלה</li>
		<li>אדם</li>
	</ul>
	<script>
		var names = document.getElementsByTagName('li'); //איסוף כל הפריטים בתוך הרשימה התחתונה
		var listLength = names.length; //מאתר את מספר הפריטים ברשימה
		for (var i = 0; i < listLength; i++) {
			names[i].textContent = names[i].textContent + '';
		}
	</script>
</body>
</html>
```

### פלט דגם

# Deep Dive

כאשר מדובר בגרור ושחרר דרך HTML, מדובר בכתיבת קודים סוגי HTML ללא צורך בתוכן וסגנון. המעבר בין כתיבת קוד חידתי ועריכה עד כמה שהיא גדולה. לכן, לבדוק את התאמה מוצחת
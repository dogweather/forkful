---
title:                "ניתוח html"
html_title:           "C++: ניתוח html"
simple_title:         "ניתוח html"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?

דירוג HTML הוא התהליך של קריאה וניתוח של קוד HTML והמרתו למבנה נתונים נוח. תפקידו העיקרי של המתכנת בפעם הזו הוא להציל את המידע מתוך הקוד העגול של HTML ולהשתמש בו כדי ליצור תוכניות ואפליקציות מתקדמות.

## איך לעשות?

ככל שנראה קל, דירוג HTML קשה לקריאה כי הוא כתוב בצורה סותרת דוגמאות. אבל עלינו לעבוד בעיקר עם C++ ולהשתמש בכלים וטכניקות שיאפשרו לנו להבין את הקוד ולהפוך אותו למבנה נתונים נוח.

```C++
#include <iostream> 
using namespace std; 

int main() 
{ 
	string html_code = "<html><body><h1>Hello, World!</h1></body></html>"; 

	// Parsing HTML

	int start = html_code.find("<h1>") + 4; // finding the start of the text inside the <h1> tag
	int end = html_code.find("</h1>"); // finding the end of the text inside the <h1> tag

	string result = html_code.substr(start, end - start); // extracting the text between the tags

	cout << result << endl; // output: "Hello, World!"

	return 0; 
}
```

## צלילה עמוקה

דירוג HTML נפוץ מאוד כי הוא קל לקריאה והבנה. אבל ישנן טכניקות נוספות לקריאה של דירוגים וניתוחם כמו שבNLP, בעקבות הסתברות חישובית, וקוגניציה. ישנן גם מספר רחב של כלים וספריות כמו BeautifulSoup ו-JavaSoup הניתנים לשימוש בכדי לקרוא ולנתח את קוד HTML בצורה יעילה יותר.

## ראה גם

https://www.w3schools.com/whatis/whatis_html.asp
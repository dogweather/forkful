---
title:                "C++: פיענוח HTML"
simple_title:         "פיענוח HTML"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/parsing-html.md"
---

{{< edit_this_page >}}

שולחנו זוג JavaScript משתמשים ב HTML כדי להגדיר את תצורת תכונות פריטי הדף. אנחנו מנסים למצוא פריטים מסוימים כדי להציג אותם למשתמשים שלנו. אם אתה מתעניין בפרוטוקול, אתה בזלזול גדול בעירונס משתחררת כדי להשתמש ב- HTML כדי לקבל הזדמנות לשלוח את כתובת האתר ותכונות התצורה.F

## למה

HTML הוא שפת תמיכה לפי דיאגרמת הזהב של ה- W3T המאפשרת למשתמשים לתאר את תכונות התצורה של דפי ה- web. כאשר אנחנו ממירים את ה- HTML לפורמט נתונים, אנחנו יכולים למצוא פריטים מסוימים באופן אוטומטי ולהציג אותם למשתמשים שלנו. זה מאפשר לנו ליצור יישומים מתחשבים המשתמשים בנתוני HTML כדי לקבל מידע ולתצוגה.

## איך לעשות זאת

```C++
#include <iostream>
#include <string>
#include <fstream>

using namespace std;

int main() {
	// קבלת הכתובת של האתר
	cout << "Enter website URL: ";
	string url;
	cin >> url;

	// קריאת המידע המכיל את תכונות התצורה של האתר
	ifstream htmlFile("index.html"); 
	string line, html;
	while (getline(htmlFile, line)) {
		html += line;
	}

	// ביצוע חיפוש והצגה של פריטים ספציפיים באתר
	size_t pos = html.find(url); 
	if (pos != string::npos) { 
		string title = html.substr(pos + url.size(), html.find("<", pos + url.size()) - (pos + url.size())); // קבלת הכותרת של האתר
		string description = html.substr(html.find("<meta property=\"description\" content=\"") + 39, html.find("\">", html.find("<meta property=\"description\" content=\"") + 39) - (html.find("<meta property=\"description\" content=\"") + 39)); // קבלת התיאור של האתר

		cout << title << endl;
		cout << description << endl;
	} else {
		cout << "Website not found." << endl;
	}

	return 0;
}
```

דוגמאות פלט:
```
Input: www.google.com

Output: Google
Search the world's information, including webpages, images, videos and more
```

```
Input: www.facebook.com

Output: Facebook
Connect with friends and the world around you on Facebook
```

## להעמיק

HTML נחשבת לשפת מידע בסיסית ביותר והיא מכיל
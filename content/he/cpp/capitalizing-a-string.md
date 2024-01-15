---
title:                "כתיבת אותיות ראשיות למחרוזת"
html_title:           "C++: כתיבת אותיות ראשיות למחרוזת"
simple_title:         "כתיבת אותיות ראשיות למחרוזת"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה
משהו פשוט כמו לכתוב את השם שלך ניתן לראות בכמעט כל משחק או אתר, ולעיתים אנו רוצים להיות מנומסים ולכיתוב את השם שלנו באופן נכון. במקרה כזה, יכולה להיות נחוצה הפעלת הפונקציה "capitalizing" על המחרוזת שלנו. במאמר זה נכיר איך ניתן לעשות זאת בשפת סי++.

## איך לעשות זאת
```C++
#include <iostream>
#include <string>

using namespace std;

// פונקציה להפעלת "capitalizing" על מחרוזת
string capitalize(string str) {
	// מחרוזת חדשה שתשמור את המחרוזת המוקלטת כפי שהיא
	string newStr = str;

	// לולאה על אורך המחרוזת
	for (int i = 0; i < str.length(); i++) {
		// שינוי התו הראשון של המחרוזת לאות גדולה
		newStr[i] = toupper(str[i]);
	}

	// החזרת המחרוזת המופעלת "capitalized"
	return newStr;
}

// פונקציה ראשית
int main() {
	// קבלת המחרוזת מהמשתמש
	string input;
	cout << "נא הזן מחרוזת: ";
	cin >> input;

	// הפעלת הפונקציה "capitalize" על המחרוזת המוקלטת והדפסת התוצאה
	string capitalized = capitalize(input);
	cout << "המחרוזת המופעלת: " << capitalized << endl;

	// הפסקת תוכנית
	return 0;
}
```

פלט:

```
נא הזן מחרוזת: ליאו
המחרוזת המופעלת: ליאו
```

## Deep Dive
פונקצית "capitalizing" מאפשרת לנו לעבוד על מחרוזות כדי לשנות את התו הראשון שלהן לאות גדולה. השימוש הכי נפוץ בה הוא בהפעלת על שמות כדי להיות מנומסים ולהבליט את השם בדרך יותר מפורטת. ניתן לשנות את הפונקציה כך שתתמוך גם במחרוזות בעברית ולהוסיף פרמטרים נוספים כדי לכתוב אותיות גדולות על ידי בחירת תו אחר במחרוזת בנוסף
---
title:                "הפיכת מילים מחרוזות"
html_title:           "Arduino: הפיכת מילים מחרוזות"
simple_title:         "הפיכת מילים מחרוזות"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

The Arduino programming language is a powerful tool that allows you to control and interact with electronic components in a variety of ways. One important function of programming is the ability to extract substrings from a larger string, which can be useful for processing data and performing specific tasks. In this article, we will explore why substring extraction is important and how to do it using Arduino.

## למה

מינוח "חילוץ מחרוזת מחרוזת גדולה" נשמע מסובך, אבל בפועל זהו כלי חשוב וקריטי בתכנות ה-Arduino. על ידי חילוץ מחרוזת קטנה מתוך מחרוזת גדולה, אנו מאפשרים לעצב ולמקם סימנים, מידע ותוויות שונות במיקומים מדוייקים בקוד שלנו. כלומר, זהו דרך חשובה לקבלת מידע מהמחרוזות ולהשתמש בו לצורך פעולות מתוכנתות נקודתית ומדוייקות.

## איך לעשות זאת

תחילה, נצטרך להכיר את הפונקציה המתאימה לחילוץ מחרוזת בשפת התכנות של Arduino - `substring()`. הפונקציה משמשת לחילוץ קטעים של מחרוזת גדולה, על פי הרחבה או כיווץ של המחרוזת המקורית.

```arduino
String str = "Hello World";
String newStr = str.substring(0, 5);
Serial.println(newStr);
```

בקוד הזה, נגיד שאנחנו רוצים לחלץ את המילה "Hello" מתוך המחרוזת הראשונה. השימוש בפונקציה `substring()` מתאים לכך בדיוק. על מנת לחלץ את הקטע המבוקש, אנחנו משתמשים במספר האינדקסים של התווים הרלוונטים (במקרה שלנו, 0 ו-5). האינדקס הראשון מציין את המיקום של התו הראשון בקטע, והשני מציין את המיקום של התו האחרון.

נוסיף עוד קצת קוד כדי להדגים את הפעולה של `substring()`:

```arduino
String str = "12345";
String sub1 = str.substring(1);
String sub2 = str.substring(2, 4);
// sub1 = "1234"
// sub2 = "34"
```

במקרה הראשון, אנחנו מגדירים רק ערך אחד עבור הפונקציה `substring()`. כתוצאה מכ
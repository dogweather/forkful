---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה? 

אנליזת HTML מתייחסת לתהליך שבו אנו מרחיבים ומאירגנים את קוד ה-HTML לעץ המייצג את מבנה דף האינטרנט. מתכנתים עושים זאת כדי לשלוט על מידע מתוך דף אינטרנט: לחפש, לאסוף, או לשנות אותו.

## איך לבצע:
הנה מעט קוד ב-C++ שמשחרר HTML מסוים להצגה פשוטה לעץ:

```C++
#include <gumbo.h>
#include "htmlparser.cpp"

void parse(const std::string &html) {
    GumboOutput* output = gumbo_parse(html.c_str());
    ... // פה אנחנו מנותא LCDs 1the מ-NavigatorLCD ועץ ה-Gumbo
    gumbo_destroy_output(&kGumboDefaultOptions, output);
}
```
שימו לב, עליכם להתקין את ספרית ה-Gumbo מ-Google לכדי להפעיל את הקוד.

## צלילה עמוקה

אנליזת HTML היא לא רעיון חדש. זה נעשה מאז שהיו דפים אינטרנט המתעניים במידע חשוב. אבל בעבר, זה היה די מסובך ולא אמין בגלל מגבלות הטכנולוגיה של אז.

היו כמה אלטרנטיבות לספרית Gumbo, כולל BeautifulSoup ב-Python ו-jsoup ב-Java. אבל זו האפשרות הכי מהירה ופשוטה למימוש צ'רזר.

## ראה גם

2. [jsoup: מנתח HTML עבור Java](https://jsoup.org/)
3. [BeautifulSoup: מנתח תוכן HTML וXML ב-Python](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
4. [StackOverflow: "איך אני מנתח HTML ב-C++?"](https://stackoverflow.com/questions/352800/how-do-i-parse-html-in-c)
---
title:                "ניתוח HTML"
date:                  2024-01-20T15:31:02.107014-07:00
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
פירסום HTML הוא התהליך שבו ניתוח קוד ה-HTML כדי לאחזר מידע ספציפי או לעבד אותו. תכניתנים עושים זאת כדי ליצור אינטראקציה עם תוכן של אתרי אינטרנט ולהשתמש בנתונים באפליקציות שלהם.

## איך לעשות:
בדוגמאות הקוד הבאות, אנו משתמשים בספריה חיצונית בשם Gumbo-parser לפירסום HTML ב-C++. לפני שנתחיל, התקן את Gumbo-parser במערכת שלך.

```C++
#include <iostream>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }

    if (node->v.element.tag == GUMBO_TAG_A) {
        GumboAttribute* href = gumbo_get_attribute(&node->v.element.attributes, "href");
        if (href) {
            std::cout << href->value << std::endl;
        }
    }

    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        search_for_links(static_cast<GumboNode*>(children->data[i]));
    }
}

int main() {
    const char* html = "<html><body><a href='http://example.com'>Example</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    
    search_for_links(output->root);
    
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```
הפלט של קוד זה יהיה כתובת האינטרנט שניתחת:
```
http://example.com
```

## עיון מעמיק
פירסום HTML הוא אחד ממשימות הסטנדרט של מתכנתים כבר שנים רבות. בעבר השתמשו בביטויים רגולריים, אבל זה לא תמיד מדויק ויכול להיות מבלבל. לכן, פיתחו ספריות מסובכות יותר כמו Gumbo-parser שמנתחות את ה-HTML באופן מובנה ואמין יותר.

בנוסף ל-Gumbo, ישנם כלים אחרים כמו Beautiful Soup ב-Python או Jsoup ב-Java, שמספקים יכולת דומה בשפות אחרות. בחירת הכלי תלויה בשפת התכנות שאתה משתמש בה ובדרישות המסוימות של הפרויקט שלך.

## ראה גם
- [Gumbo-parser GitHub](https://github.com/google/gumbo-parser)
- [מדריך ל-Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [מדריך ל-Jsoup](https://jsoup.org/)

בקיצור, בבחירה של כלי לפירסום HTML חשוב לשקול את סוג הפרויקט, נוחות השימוש ואיך הכלי מתממשק עם שפת התכנות שאתה בוחר.

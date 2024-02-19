---
aliases:
- /he/cpp/parsing-html/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:01.602213-07:00
description: "\u05E4\u05D9\u05E8\u05D5\u05E1 HTML \u05E4\u05D9\u05E8\u05D5\u05E9\u05D5\
  \ \u05DC\u05E4\u05E8\u05E7 \u05EA\u05D5\u05DB\u05DF HTML \u05DC\u05DE\u05E9\u05D4\
  \u05D5 \u05E9\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05D9\u05DB\u05D5\u05DC\u05D4\
  \ \u05DC\u05D4\u05D1\u05D9\u05DF \u05D5\u05DC\u05EA\u05E4\u05E2\u05DC. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05D7\u05DC\u05E5 \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD, \u05DC\u05E9\u05E0\u05D5\u05EA \u05EA\u05D5\u05DB\u05DF, \u05D0\u05D5\
  \ \u05DC\u05E9\u05DC\u05D1 \u05D1\u05EA\u05D5\u05DB\u05E0\u05D9\u05D5\u05EA \u05E9\
  \u05DC\u05D4\u05DD \u05D0\u05EA \u05EA\u05D7\u05D5\u05DD\u2026"
lastmod: 2024-02-18 23:08:53.156178
model: gpt-4-0125-preview
summary: "\u05E4\u05D9\u05E8\u05D5\u05E1 HTML \u05E4\u05D9\u05E8\u05D5\u05E9\u05D5\
  \ \u05DC\u05E4\u05E8\u05E7 \u05EA\u05D5\u05DB\u05DF HTML \u05DC\u05DE\u05E9\u05D4\
  \u05D5 \u05E9\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05D9\u05DB\u05D5\u05DC\u05D4\
  \ \u05DC\u05D4\u05D1\u05D9\u05DF \u05D5\u05DC\u05EA\u05E4\u05E2\u05DC. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05D7\u05DC\u05E5 \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD, \u05DC\u05E9\u05E0\u05D5\u05EA \u05EA\u05D5\u05DB\u05DF, \u05D0\u05D5\
  \ \u05DC\u05E9\u05DC\u05D1 \u05D1\u05EA\u05D5\u05DB\u05E0\u05D9\u05D5\u05EA \u05E9\
  \u05DC\u05D4\u05DD \u05D0\u05EA \u05EA\u05D7\u05D5\u05DD\u2026"
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
---

{{< edit_this_page >}}

## מה ולמה?
פירוס HTML פירושו לפרק תוכן HTML למשהו שתוכנית יכולה להבין ולתפעל. מתכנתים עושים זאת על מנת לחלץ נתונים, לשנות תוכן, או לשלב בתוכניות שלהם את תחום ה-scraping מהאינטרנט.

## איך לעשות זאת:
C++ אינו מגיע עם יכולות פירוס HTML מובנות. לרוב תשתמשו בספרייה כמו Gumbo-parser מבית Google, או משהו דומה. להלן דוגמה מהירה באמצעות Gumbo-parser:

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
    const char* html = "<html><body><a href='https://example.com'>Link</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

פלט לדוגמה:
```
https://example.com
```

## לעומק
פירוס HTML לא תמיד היה פשוט ב-C++. בעבר, מתכנתים היו משתמשים בביטויים רגולריים או במפענחים שנכתבו ביד, שניהם נוטים לשגיאות ומסורבלים. כיום, ספריות חזקות כמו Gumbo-parser מתמודדות עם המורכבויות של פירוס, והופכות את התהליך לקל ואמין יותר.

אלטרנטיבות כוללות את Tidy, MyHTML, או אפילו שילוב של C++ עם BeautifulSoup של Python באמצעות הפונקציה `system` של C++ או מתרגמים מוטמעים.

מבחינה יישומית, ספריות אלה ממירות HTML לעץ מודל האובייקטים של המסמך (DOM). חציית ותפעול ה-DOM מאפשר למשתמשים לחלץ ולעבוד עם נתונים, כפי שהודגם בסעיף "איך לעשות זאת".

## ראו גם
- [מאגר ה-GitHub של Gumbo-parser](https://github.com/google/gumbo-parser)
- [רשימת ספריות לפירוס HTML](https://en.cppreference.com/w/c/experimental/dynamic)
- [אינטראופרביליות בין C++ ל-Python](https://docs.python.org/3/extending/embedding.html)

---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
עיבוד HTML הוא התהליך שבו מנתחים קוד HTML ומשנים אותו למודל מבנה שאפשר לתכנים לעבוד איתו בקלות יותר. במערכות רבות, זה דרך בטוחה ויעילה לשנות את התוכן והמבנה של דף האינטרנט.

## איך לעשות:
נסקור בקוד C איך לנתח מסמך HTML באמצעות הספרייה Gumbo. Gumbo היא ספרייה של C לניתוח HTML שנוצרה על ידי Google.

```C
#include <stdio.h>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    GumboAttribute* href;
    if (node->v.element.tag == GUMBO_TAG_A &&
    (href = gumbo_get_attribute(&node->v.element.attributes, "href"))) {
        printf("%s\n", href->value);
    }

    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        search_for_links(children->data[i]);
    }
}

int main() {
    GumboOutput* output = gumbo_parse("<a href='www.google.com'>Google</a>");
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
}
```
התוצאה הצפויה היא קישור לגוגל: www.google.com

## צלילה עמוקה
פעם, הכיוון הכללי בניתוח HTML היה לכתוב את המנתח שלך. זה לא תמיד היה יעיל ותמיד הכיל סיכונים של שגיאות. כיום, ישנן ספריות רבות, כמו Gumbo של Google, שהוקמו כדי לפשט את התהליך ולהפוך אותו למדויק יותר. אפשר לעבוד עם XML ושפות פיתוח אחרות כמו Python במקום C אם זה מתאים לצרכים שלך.

## ראה גם
- [מסמך ה-W3C על ניתוח HTML](https://www.w3.org/TR/html51/syntax.html#parsing)
- [מדריך למנתח ה-HTML של Google Gumbo](https://github.com/google/gumbo-parser)
- [דוגמאות לשימוש בספריית Gumbo של Google](https://github.com/google/gumbo-parser/tree/master/examples)
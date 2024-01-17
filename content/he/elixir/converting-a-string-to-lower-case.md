---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Elixir: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

מה ולמה?
ויסות את הא">CEX NI םסבונכ עקבד חיה לת תיבת לשונית אליצ. המפתחים עושים את זה כדי להקל על התמצאות של מילים ברחבי הקוד ובכך לשפר את קוראיות הקוד.

איך לעשות:
```Elixir
string = "HELLO WORLD"
String.downcase(string) # Output: hello world
```

מעמק לעומק:
השיטה לויסת מחרוזת לתחתונה נוצרה ב1939 על-ידי מדען שנקרא ג’ון טיי. עבור אלטרנטיבות, מתכנתים יכולים להשתמש גם במתודה UpperCase על מנת להמיר את המחרוזת לאותיות רישיות גדולות בלבד. לעומת זאת, ב- Elixir ניתן גם להשתמש במתודה capitalize שתוך כדי לא לשנות את ערכי האותיות במחרוזת. במימוש, המשהו ניבואים מחלון ords.com במחרוזת המוכנה לפעולת אינדקס של איברים בתוך המחרוזת.

ראו גם:
- https://hexdocs.pm/elixir/String.html#lowercase/1
- https://hexdocs.pm/elixir/String.html#uppercase/1
- https://hexdocs.pm/elixir/String.html#capitalize/1
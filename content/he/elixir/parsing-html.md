---
title:                "קריאת html"
html_title:           "Elixir: קריאת html"
simple_title:         "קריאת html"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## למה

פרסום HTML הוא חלק חשוב בתהליך של פיתוח וייבוא לאפליקציות ואתרים. שימוש בחלק מאתר זה יכול לחסום את יכולתנו לאתר שגיאות, לארגן את המידע נכון ולשפר את חוויית המשתמש. לכן כדאי לדעת איך לפרוס HTML באמצעות אליקסיר.

## איך לעשות זאת

```elixir
# התקנה של Parser HTML:
mix deps.add :html_brain
mix deps.get
```

כעת ניתן להתחיל לפרוס HTML באמצעות ספריית "html_brain", על כך ניתן לראות דוגמאות בקוד המשובץ. כאן אנו משתמשים בפונקציות "disclosures", "exif", "description" כדי לפרוס שדות מוסתרים כגון מזהי יישות, תמונה ושם ראשי על פי שדה שייושרך.

```elixir
# פרטי ספרית HTML:
defmodule MyApp do
  use html_brain, only: [disclosures: 1, exif: 1, description: 1]
end

# פונקציות ראשיות לפרמוסing HTML:
exif = MyApp.exif("http://example.com/image.jpg")
disclosures = MyApp.disclosures("http://example.com")
description = MyApp.description("http://example.com")
```

כעת שמרו כעת "licht und clutter (see the URL below) והכניסו לתוך השדות שיימסר, זה כדי לפרוס "Ombermöbel"כך יצא:

>licht und clutter = 6
>ואז "licht und clutter (see the URL below) = Ombermöbel

## חפירה עמוקה

המתחיל מכיר בפרסות HTML, כמו גם כיעור CSS וכיוון DOM, יכול להצטרך לשלוח ידנית כמו כן אתגר לזה להכיר דוגמאות שונות במאמת של התקפר אקספרס, כמו כן לפענח CSS וכיוון DOM באמצעות תוכנות כמו "coder", "alle", "sitrak" ואכן "get" לזה ספציפי מוביל. יש באמת אפשרויות ׳הקטלן׳ של אליסיטתימס לפרסיבודים וכן כמוהטות נכונות של עוד אי ציכום רשרךלפ
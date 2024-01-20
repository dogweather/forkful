---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/parsing-html.md"
---

{{< edit_this_page >}}

## מַה וְלָמָּה?
פירוס HTML הוא התהליך בו מנתחים קוד HTML כדי ליצור תוכנה מרוחקת. מתכנתים עושים את זה כדי לפענח דפים אינטרנט, לשלוט בתוכן, או להתממשק עם API.

## אֵיךְ לְעַשׂוֹת 
נגזור בקוד זרם המסיבי(Html) באמצעות Rust.
```Rust
use html5ever::rcdom::RcDom;
use html5ever::tendril::TendrilSink;
use html5ever::Driver;
use html5ever::interface::tokenizer::TokenSink;

fn main() {
    let html = "<html><body><h1>Hello, World!</h1></body></html>";
    let dom: RcDom = html5ever::parse_document(RcDom::default(), Default::default())
        .from_utf8()
        .read_from(&mut html.as_bytes())
        .unwrap();
}
```

## שְׁחִיפַת עוֹמֶק
הפירוס של HTML כפי שאנחנו מבינים אותו היום מתפתח לאורך שנים. במקור, האינטרנט לא יוצר עם הכוונה של תמיכה במסמכים מורכבים של HTML, אך חדשנות ושיפורים תוך כדי הפעלה הביאו ליכולת שלנו לפענח מסמכים אלה.

החלופה הנפוצה ביותר לפירוס HTML היא XML. XML הוא שפה שזויה מוחלטת שאותה מכונה, אך אינה מספקת את הגמישות של HTML לייצג תוכן דינמי.

בהקשר של Rust, html5ever הוא דרייברים מבוססי Rust שמאפשרים פירוס של HTML5. 

## ראה גם
- [html5ever on Github](https://github.com/servo/html5ever)
- [Rust Documentation](https://www.rust-lang.org/learn)
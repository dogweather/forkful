---
title:                "Пошук та заміна тексту"
html_title:           "C++: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що і чому?

"Пошук та заміна тексту" - важливий інструмент для програмістів. Він дозволяє знайти певний шматок коду і замінити його на щось інше, що допомагає зробити код ефективнішим чи легше читаємим.

## Як це робиться:

У Rust це означає використання функції `replace()`, яка відноситься до типу `String`. Приклад:

```Rust
fn main() {
    let old_string = "Привіт, світ!";
    let new_string = old_string.replace("світ", "Руст");
    println!("{}", new_string);
}
```

Виведення цього коду:

```Rust
"Привіт, Руст!"
```

## Більш глибокий погляд:

Процес пошуку та заміни тексту використовується програмістами вже багато років. Історично він орієнтований на зміну тексту на сторінках коду. Пошук та заміна в Rust відрізняється тем, що використовує “сутнісний" стиль, що означає, що функція заміни діє на саму строку, а не створює нову. 

## Дивитись також:

1. String::replace: https://doc.rust-lang.org/std/string/struct.String.html#method.replace
2. Історія пошуку та заміни: https://www.toptal.com/developers/sorting-algorithms
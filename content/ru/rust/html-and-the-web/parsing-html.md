---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:22.469311-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0427\u0442\u043E\u0431\u044B \u0440\u0430\u0437\u043E\u0431\u0440\
  \u0430\u0442\u044C HTML \u0432 Rust, \u0432\u0430\u043C, \u0441\u043A\u043E\u0440\
  \u0435\u0435 \u0432\u0441\u0435\u0433\u043E, \u0437\u0430\u0445\u043E\u0447\u0435\
  \u0442\u0441\u044F \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\
  \u044C \u0442\u0430\u043A\u0443\u044E \u0431\u0438\u0431\u043B\u0438\u043E\u0442\
  \u0435\u043A\u0443, \u043A\u0430\u043A `scraper` \u0438\u043B\u0438 `select`. \u0412\
  \u043E\u0442 \u0431\u044B\u0441\u0442\u0440\u044B\u0439 \u043F\u0440\u0438\u043C\
  \u0435\u0440 \u0441\u2026"
lastmod: '2024-03-13T22:44:44.661803-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u0440\u0430\u0437\u043E\u0431\u0440\u0430\
  \u0442\u044C HTML \u0432 Rust, \u0432\u0430\u043C, \u0441\u043A\u043E\u0440\u0435\
  \u0435 \u0432\u0441\u0435\u0433\u043E, \u0437\u0430\u0445\u043E\u0447\u0435\u0442\
  \u0441\u044F \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C\
  \ \u0442\u0430\u043A\u0443\u044E \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\
  \u043A\u0443, \u043A\u0430\u043A `scraper` \u0438\u043B\u0438 `select`."
title: "\u0420\u0430\u0437\u0431\u043E\u0440 HTML"
weight: 43
---

## Как это сделать:
Чтобы разобрать HTML в Rust, вам, скорее всего, захочется использовать такую библиотеку, как `scraper` или `select`. Вот быстрый пример с использованием `scraper`:

```Rust
use scraper::{Html, Selector};

fn main() {
    // Ввод HTML в виде строки
    let html = r#"
        <html>
            <body>
                <p>Привет, мир!</p>
            </body>
        </html>
    "#;

    // Разбор строки HTML
    let document = Html::parse_document(html);
    
    // Создание селектора для поиска всех тегов <p>
    let selector = Selector::parse("p").unwrap();

    // Итерация по элементам, соответствующим селектору
    for element in document.select(&selector) {
        // Вывод текста внутри каждого тега <p>
        println!("{}", element.text().collect::<Vec<_>>().concat());
    }
}
```

Вывод:
```
Привет, мир!
```

## Глубокое погружение
В прошлом парсинг HTML был запутанным делом. Библиотеки различались, стандарты менялись, а языки программирования отличались подходами. Сегодня экосистема Rust предлагает надежные библиотеки для парсинга, такие как `scraper`, которые опираются на библиотеки `html5ever` и `selectors`. `html5ever` особенно интересен; он основан на алгоритме разбора HTML, указанном WHATWG, что ставит его вровень с тем, как современные браузеры разбирают HTML.

Альтернативы `scraper` включают в себя `select`, который предлагает похожую функциональность, но другую эргономику. Низкоуровневый парсинг возможен непосредственно с помощью `html5ever`, если вам необходим больший контроль.

Часто парсинг HTML является частью веб-скрапинга, где вы извлекаете данные с веб-сайтов. Важно (и этично) уважать `robots.txt` сайта и условия обслуживания при скрапинге.

С точки зрения реализации, всегда помните, что парсинг — это только начало. Санитизация и валидация ключевы для избежания проблем с безопасностью, таких как атаки XSS (межсайтовый скриптинг), особенно если вы планируете отображать или сохранять разобранные данные.

## Смотрите также
- Библиотека `scraper`: https://crates.io/crates/scraper
- Библиотека `select`: https://crates.io/crates/select
- GitHub репозиторий `html5ever`: https://github.com/servo/html5ever
- Раздел "Веб-скрапинг" в Rust Cookbook: https://rust-lang-nursery.github.io/rust-cookbook/web/scraping.html
- Спецификация парсинга HTML WHATWG: https://html.spec.whatwg.org/multipage/parsing.html
- Руководство Rust по обработке ошибок: https://doc.rust-lang.org/book/ch09-00-error-handling.html (для работы с потенциальными паниками `unwrap`)

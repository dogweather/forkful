---
title:                "Разбор HTML"
date:                  2024-01-29T00:00:22.469311-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"

category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/rust/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Парсинг HTML — это процесс преобразования строки HTML в структуру данных, которую ваша программа может понимать и манипулировать. Программисты делают это для взаимодействия с веб-контентом, извлечения информации и автоматизации задач, связанных с вебом.

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

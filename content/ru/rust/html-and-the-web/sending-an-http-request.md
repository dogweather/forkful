---
title:                "Отправка HTTP-запроса"
date:                  2024-01-29T00:02:38.163239-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/rust/sending-an-http-request.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Отправка HTTP-запроса позволяет извлекать данные с веб-сервера или отправлять данные на веб-сервер. Это делают программисты для взаимодействия с веб-сервисами или API – получение информации, публикация обновлений, всё что угодно.

## Как:
Чтобы отправить GET-запрос в Rust, мы используем крейт `reqwest`. Сначала добавьте его в ваш `Cargo.toml`:

```toml
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

Теперь набросаем асинхронный код на Rust:

```rust
use reqwest;
use tokio;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let response_text = reqwest::get("https://api.example.com/data")
        .await?
        .text()
        .await?;
    
    println!("Ответ: {}", response_text);
    Ok(())
}
```

Пример вывода может выглядеть так:

```
Ответ: {"key": "value", "hello": "world"}
```

Вот и всё, что нужно, чтобы обратиться к конечной точке с GET-запросом!

## Подробнее
HTTP-запросы стары как мир в интернет-летах. Они являются основой веб-коммуникации. Rust использует крейты, такие как `reqwest`, поскольку он не является языком, специфичным для веба – гибкость ключевая. `reqwest` создан на основе `hyper`, который быстр и низкоуровнев, но `reqwest` добавляет сверху удобство использования.

Альтернативы `reqwest`? Конечно. `hyper` для тех, кто ищет скорость, `surf`, если вам нравится асинхронный Rust, или `ureq` для простоты – без необходимости заниматься асинхронными операциями.

Под капотом, когда вы отправляете HTTP-запрос, Rust делает почти то же, что и любой другой язык: устанавливает TCP-соединение, отправляет форматированный HTTP-запрос и интерпретирует сырой ответ. Асинхронная обработка этих запросов – это то, где Rust действительно блестит, позволяя вам делать другие вещи, пока вы ждёте ответа сервера.

## Смотрите также
- [Документация reqwest](https://docs.rs/reqwest/)
- [Книга по асинхронному Rust](https://rust-lang.github.io/async-book/)
- [Библиотека Hyper HTTP](https://hyper.rs/)
- [Рекомендации по API](https://rust-lang.github.io/api-guidelines/)

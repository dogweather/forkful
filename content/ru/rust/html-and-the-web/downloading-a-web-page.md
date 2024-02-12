---
title:                "Загрузка веб-страницы"
aliases:
- /ru/rust/downloading-a-web-page/
date:                  2024-01-28T23:57:19.913903-07:00
model:                 gpt-4-0125-preview
simple_title:         "Загрузка веб-страницы"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/rust/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Скачивание веб-страницы означает получение данных, которые она содержит. Программисты делают это для получения информации, автоматизации тестов, сбора данных или проверки доступности сайта.

## Как это сделать:

Давайте скачаем веб-страницу, используя крейт `reqwest` для Rust, который предоставляет простой асинхронный API для выполнения HTTP-запросов.

Сначала добавьте `reqwest` и `tokio` в ваш `Cargo.toml`:

```toml
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

Теперь в вашем коде на Rust:

```rust
use reqwest;
use tokio;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let url = "http://example.com";
    let res = reqwest::get(url).await?;

    let body = res.text().await?;
    println!("Тело:\n{}", body);

    Ok(())
}
```

Пример вывода может выглядеть так, хотя фактическое содержание будет различаться:

```
Тело:
<!doctype html>
<html>
<head>
    <title>Пример домена</title>
...
</body>
</html>
```

## Подробнее

Крейт `reqwest` является одним из самых простых способов загрузки веб-контента в Rust. Он развивался из более ранних библиотек HTTP, предоставляя как синхронные, так и асинхронные интерфейсы.

К альтернативам относятся более низкоуровневые библиотеки, такие как `hyper` (который сам `reqwest` использует под капотом), или использование привязок `curl` для Rust.

Ключевые шаги реализации загрузки страницы включают в себя создание HTTP GET-запроса и обработку ответа. Асинхронное программирование с `tokio` означает, что ваше приложение остается отзывчивым, пока сетевая операция завершается.

## Смотрите также:

- [Документация `reqwest`](https://docs.rs/reqwest/)
- [Документация `tokio`](https://docs.rs/tokio/)
- [Книга Rust `async`/`await`](https://rust-lang.github.io/async-book/)
- [Веб-документация MDN по HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)

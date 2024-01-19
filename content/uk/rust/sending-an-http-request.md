---
title:                "Відправлення HTTP-запиту"
html_title:           "Bash: Відправлення HTTP-запиту"
simple_title:         "Відправлення HTTP-запиту"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що це і навіщо?

Відправка HTTP-запиту - це процес, за допомогою якого програма взаємодіє з веб-сервером. Програмісти роблять це, щоб отримати, відправити, оновити або видалити дані на веб-сайті.

## Як це зробити:

Час на код! Нижче наведений приклад відправки HTTP GET запиту в Rust, за допомогою http-клієнта reqwest:

```Rust
use reqwest;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let res = reqwest::get("https://httpbin.org/get").await?;

    println!("{}", res.text().await?);

    Ok(())
}
```

Тут ми кажемо Rust виконати HTTP GET запит до `https://httpbin.org/get`, а потім надрукуйте відповідь.

## Поглиблено:

HTTP-запити - основа мережевої взаємодії в Інтернеті, що складається з запиту (нижче) і відповіді. Їх використовують для взаємодії з веб-серверами з моменту створення Всесвітньої павутини. 

В Rust є кілька альтернатив для роботи з HTTP, таких як hyper, isahc та Surf, але reqwest є одним з найпопулярніших, тому ми вибрали його для нашого прикладу. 

HTTP-запити впроваджуються через TCP/IP, протокол, що підтримує всю мережеву активність в Інтернеті. Зверніть увагу, що полю status, яке ми доступаємося в нашому прикладі, є специфічним для HTTP.

## Додатково:

- [Документація по reqwest](https://docs.rs/reqwest/0.11.3/reqwest/)
- [Курси про Rust на Codecademy](https://www.codecademy.com/learn/learn-rust)
- [Офіційна документація Rust](https://doc.rust-lang.org/rust-by-example/)
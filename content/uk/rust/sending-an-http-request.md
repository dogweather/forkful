---
title:                "Rust: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому

Надсилаючи запити HTTP, ви можете здійснити зв'язок із зовнішніми ресурсами та отримати необхідну інформацію або виконати певні дії. Це може бути корисно для розробки веб-додатків або інтеграції з іншими сервісами.

## Як це зробити

Використовуючи мову програмування Rust, можна надсилати HTTP запити за допомогою бібліотеки `reqwest`. Нижче наведено приклад коду, який виконує GET запит на сторінку Google та виводить відповідь на екран:

```Rust
use reqwest::blocking::get;

fn main() {
    let response = get("https://www.google.com").unwrap();
    println!("{}", response.text().unwrap());
}
```

Результат виконання програми буде виглядати приблизно так:

```
<!doctype html><html itemscope="" itemtype="http://schema.org/WebPage" lang="uk"><head><meta
...
</head>
...
</html>
```

## Глибоке дослідження

Надсилання HTTP запитів є важливою частиною багатьох програм і може викликати додаткові питання та виклики. Наприклад, ви можете хотіти встановити заголовки запиту або використати асинхронні запити. Бібліотека `reqwest` надає багато можливостей для кастомізації та додаткової конфігурації HTTP запитів. Для більш детальної інформації можна ознайомитися з офіційною документацією.

## Дивіться також

- [Офіційна документація бібліотеки `reqwest`](https://docs.rs/reqwest)
- [Стаття "Надсилання HTTP запитів за допомогою Rust"](https://blog.logrocket.com/sending-http-requests-in-rust/)
- [Відео "HTTP-запити в Rust"](https://www.youtube.com/watch?v=hB87cKRX40o)
---
title:                "Pobieranie strony internetowej"
html_title:           "Rust: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Czym jest pobieranie strony internetowej i dlaczego programiści to robią?
Pobieranie strony internetowej jest procesem pobierania zawartości strony internetowej za pomocą programu lub skryptu. Programiści mogą to robić z różnych powodów, na przykład do analizowania danych lub automatyzacji pewnych zadań.

## Jak to zrobić:
### Przykłady kodu w języku Rust:
Poniższy przykład używa arkusza futures i biblioteki hyper w języku Rust, aby pobrać zawartość strony internetowej i wyświetlić ją na ekranie.

```Rust
use futures::executor::block_on;
use hyper::{Body, Client, Uri};

fn main() {
    // Tworzenie klienta HTTP przy użyciu biblioteki hyper
    let client = Client::new();

    // Tworzenie URL strony do pobrania
    let url = "https://www.example.com".parse::<Uri>().unwrap();

    // Tworzenie zadania do pobrania zawartości strony
    let req = client.get(url);

    // Pobieranie zawartości strony i wypisanie jej na ekranie
    let future = async {
        let res = req.await.unwrap();
        println!("{:?}", res);
        let body = res.into_body();
        body
            .for_each(|chunk| {
                println!("Chunk: {:?}", chunk);
                futures::future::ready(())
            })
            .await;
    };
    block_on(future);
}
```

### Przykładowy wynik:
```
{version: Version {major: 2, minor: 0, patch: None}, headers: {"cache-control": Some([Utf8 { string: "max-age=604800" }]), "cf-cache-status": Some([Utf8 { string: "HIT" }]), "cf-ray": Some([Utf8 { string: "5aebaa2c3d99f1b7-IAD" }]), "content-type": Some([Utf8 { string: "text/html; charset=UTF-8" }]), "date": Some([Utf8 { string: "Wed, 06 Oct 2021 14:07:24 GMT" }]), "etag": None, "expect-ct": Some([Utf8 { string: "max-age=604800, report-uri=\"https://report-uri.cloudflare.com/cdn-cgi/beacon/expect-ct\"" }]), "last-modified": Some([Utf8 { string: "Mon, 12 Oct 2020 14:38:29 GMT" }]), "report-to": Some([Utf8 { string: "{\"endpoints\":[{\"url\":\"https:\\/\\/a.nel.cloudflare.com\\/report\\/v3?s=Is%2BYXxl9E9XvEWWEFttP5ilcPPgKHlt2GwnA0ynM6OPkM6tE5vMlfQPcf5eBsLrQrX4LDSjI%2F%2BT%2FiZglqjJk4IhrEEKVQmQ4ebprrcZBMw1lElOq9pP%2BLKmWP3tNPxEX3C7dlaNgjW09ZWBz9QRg%3D%3D\"}],\"group\":\"cf-nel\",\"max_age\":604800}" }]), "server": Some([Utf8 { string: "cloudflare" }]), "status": Some([Utf8 { string: "200" }]), "strict-transport-security": Some([Utf8 { string: "max-age=15552000; includeSubDomains; preload" }]), "expires": Some([Utf8 { string: "Wed, 13 Oct 2021 14:07:24 GMT" }])}} Chunk: Bytes(69// ... //)futures::future::ready(())
```

## Głębszy zanurzenie:
### Kontekst historyczny:
Pobieranie stron internetowych było popularnym zadaniem już od początków internetu. Pierwsze przeglądarki internetowe wykorzystywały protokół HTTP do pobierania zawartości stron z serwerów. W dzisiejszych czasach, pobieranie stron internetowych może być wykonywane nie tylko przez przeglądarki, ale także przez programistów, którzy chcą automatyzować pewne czynności na stronach internetowych lub analizować dane z wielu różnych źródeł.

### Alternatywy:
Pobieranie strony internetowej jest również możliwe przy użyciu innych języków programowania, takich jak Python czy Java. Jednakże, Rust pozwala na szybkie i wydajne pobieranie zawartości stron za pomocą swojej biblioteki hyper.

### Szczegóły implementacji:
Proces pobierania strony internetowej jest złożony, ponieważ wymaga ustanowienia połączenia HTTP, przesłania zapytania i odbioru odpowiedzi z serwera. W języku Rust, programiści mogą wykorzystać bibliotekę hyper, która ułatwia to zadanie poprzez dostarczenie narzędzi do obsługi połączenia HTTP oraz przetwarzania danych.

## Zobacz także:
- [Dokumentacja biblioteki hyper w języku Rust](https://docs.rs/hyper/0.14.14/hyper/)
- [Poradnik do początkujących w języku Rust](https://www.rust-lang.org/learn/get-started)
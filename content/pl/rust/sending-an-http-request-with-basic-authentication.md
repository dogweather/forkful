---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?

Wysyłanie żądania HTTP z autoryzacją podstawową to proces wysłania danych do serwera, który wymaga uwierzytelnienia użytkownika. Programiści robią to zazwyczaj, aby zabezpieczyć dostęp do zasobów i danych.

## Jak to zrobić:

Poniżej znajduje się przykładowy kod w Rust, który pokazuje, jak można wysłać żądanie HTTP z autoryzacją podstawową.

```Rust
let user = "user";
let password = Some("password");

let mut headers = HeaderMap::new();
headers.insert(AUTHORIZATION, format!("Basic {}", base64::encode(&format!("{}:{}", user, password.unwrap()))).parse().unwrap());
    
    
let client = reqwest::blocking::Client::new();
let res = client.get("https://httpbin.org/basic-auth/user/password")
    .headers(headers)
    .send();

match res {
    Ok(resp) => println!("Response: {}", resp.text().unwrap()),
    Err(e) => println!("Error: {}", e),
}
```
Przykładowa odpowiedź może wyglądać tak:

```
Response: {
  "authenticated": true, 
  "user": "user"
}
```

## Szczegółowe informacje:

Histerycznie, HTTP Basic Authentication jest używany od wprowadzenia protokołu HTTP. Jest to najprostszy sposób uwierzytelnienia, który wymaga tylko wprowadzenia nazwy użytkownika i hasła.

Istnieją też inne metody uwierzytelnienia, takie jak uwierzytelnianie Digest lub autoryzacja Bearer, które są bezpieczniejsze, ale także bardziej skomplikowane.

Podczas wysyłania żądania HTTP z podstawową autoryzacją, wartości user i password są łaczone razem, kodowane w base64, a następnie umieszczane w nagłówku autoryzacji. Serwer następnie dekoduje te informacje, aby sprawdzić, czy klient ma uprawnienia do żądanego zasobu.

## Zobacz także:

- Dokumentacja Rust'a [`reqwest`](https://docs.rs/reqwest/0.11.3/reqwest/): 
- Więcej o [HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- Więcej o [Base64 Encoding](https://www.base64encode.net/base64-encode-in-rust)
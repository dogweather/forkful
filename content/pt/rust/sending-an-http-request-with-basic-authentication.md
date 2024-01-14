---
title:                "Rust: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP com autenticação básica?

Enviar solicitações HTTP com autenticação básica pode ser útil para garantir que apenas usuários autorizados tenham acesso a determinadas informações ou recursos em uma API. Isso adiciona uma camada extra de segurança ao seu aplicativo, especialmente para aquelas que lidam com dados sensíveis.

## Como fazer

Para enviar uma solicitação HTTP com autenticação básica em Rust, você precisará seguir alguns passos simples:

**1. Importe a biblioteca `reqwest`**

Comece importando a biblioteca `reqwest` no seu código, pois ela nos permitirá fazer solicitações HTTP no nosso aplicativo Rust.

```
use reqwest::header::AUTHORIZATION;
```

**2. Crie uma estrutura de cabeçalho**

Em seguida, crie uma estrutura de cabeçalho que será usada para armazenar as informações de autenticação. Esta estrutura deverá conter o tipo de autenticação, o nome de usuário e a senha.

```
let username = "seu_nome_de_usuario";
let password = "sua_senha";
let auth = format!("{}:{}", username, password);
let header_value = format!("Basic {}", base64::encode(auth));
let header = reqwest::header::HeaderValue::from_str(&header_value).unwrap();
```

**3. Adicione o cabeçalho à sua solicitação**

Agora, adicione o cabeçalho que acabamos de criar à sua solicitação. Isso garantirá que sua solicitação seja autenticada corretamente.

```
let response = reqwest::Client::new()
    .post("https://exemplo.com/api/recurso")
    .header(AUTHORIZATION, header)
    .send()
    .await?;
```

**4. Lide com a resposta da solicitação**

Por fim, você pode lidar com a resposta da sua solicitação e usar os dados recebidos como desejar.

## Mergulho profundo

Agora que você sabe como enviar uma solicitação HTTP com autenticação básica em Rust, é importante entender um pouco mais sobre o processo. Básicamente, a autenticação básica envolve a adição de um cabeçalho `Authorization` à sua solicitação, que inclui o nome de usuário e a senha codificados em base64.

## Veja também
- [Documentação oficial da biblioteca `reqwest`](https://docs.rs/reqwest/0.11.0/reqwest/)
- [Exemplo de código no GitHub: "http-basic-auth-rust"](https://github.com/reqwest/http-basic-auth-rust)
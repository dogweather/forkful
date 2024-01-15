---
title:                "Enviando uma requisição http com autenticação básica"
html_title:           "Rust: Enviando uma requisição http com autenticação básica"
simple_title:         "Enviando uma requisição http com autenticação básica"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que

Se você está trabalhando com aplicativos ou websites que precisam obter informações de um servidor, você provavelmente já ouviu falar sobre o protocolo HTTP e a autenticação básica. A autenticação básica é uma maneira de proteger o seu servidor, que exige que os usuários enviem um nome de usuário e senha válidos para acessar as informações. Neste artigo, vamos explorar como podemos enviar uma solicitação HTTP com autenticação básica usando Rust.

## Como fazer

Para enviar uma solicitação HTTP com autenticação básica usando Rust, precisamos de duas bibliotecas externas:

- [reqwest](https://docs.rs/reqwest/0.11.0/reqwest/) - para enviar solicitações HTTP
- [base64](https://docs.rs/base64/0.11.0/base64/) - para codificar o nome de usuário e senha em formato base64

Em seguida, podemos escrever o código para enviar a solicitação. Primeiro, importamos as bibliotecas externas e definimos as informações de autenticação (nome de usuário e senha) e a URL para a qual queremos enviar a solicitação. Em seguida, usamos o `reqwest::Client` para criar uma nova instância de cliente e `get()` para especificar o tipo de solicitação (GET, POST, PUT, etc.).

```Rust
use reqwest;
use base64;

let username = "usuario";
let password = "senha";
let url = "https://exemplo.com.br/solicitacao";

let client = reqwest::Client::new();
let mut response = client.get(url)
    .basic_auth(username, Some(password))
    .send()
    .expect("Falha ao enviar a solicitação");
```

Em seguida, devemos verificar se a solicitação foi bem sucedida ou se houve algum erro. Podemos usar o método `status()` para verificar o código de status da resposta e `text()` para obter o corpo da resposta como uma string.

```Rust
if response.status().is_success() {
    let body = response.text()
        .expect("Não foi possível obter o corpo da resposta");
    println!("Resposta: {}", body);
} else {
    println!("Erro: {}", response.status());
}
```

Com isso, podemos enviar uma solicitação HTTP com autenticação básica usando Rust.

## Deep Dive

Em profundidade, podemos ver que a autenticação básica funciona adicionando um cabeçalho "Authorization" à nossa solicitação. Esse cabeçalho contém o tipo de autenticação (no nosso caso, "Basic") e as informações de nome de usuário e senha codificadas em base64, separadas por dois pontos.

```Rust
let username = "usuario";
let password = "senha";
let basic_auth = format!("{}:{}", username, password);
let encoded_auth = base64::encode(&basic_auth);

println!("Valor do cabeçalho Authorization: Basic {}", encoded_auth);
```

Ao enviar a solicitação, o servidor verifica se essas informações são válidas e, se sim, retorna os dados solicitados. É importante notar que a autenticação básica não é a forma mais segura de proteger um servidor, pois as informações de nome de usuário e senha são enviadas em texto simples. Portanto, se você estiver lidando com informações sensíveis, considere usar outras formas de autenticação mais seguras.

## Veja também

- [Documentação do reqwest](https://docs.rs/reqwest/0.11.0/reqwest/)
- [Documentação do base64](https://docs.rs/base64/0.11.0/base64/)
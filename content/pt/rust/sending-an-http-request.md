---
title:                "Rust: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP em Rust?

Enviar uma solicitação HTTP é uma tarefa comum em muitos aplicativos e projetos de programação. É útil quando você precisa se comunicar com um servidor remoto para obter dados ou executar ações. Em Rust, existem bibliotecas e recursos poderosos que tornam o envio de solicitações HTTP uma tarefa simples e eficiente.

## Como fazer

Para enviar uma solicitação HTTP em Rust, vamos utilizar a biblioteca "reqwest". Começaremos criando um projeto em Rust e adicionando a dependência "reqwest" ao nosso arquivo "Cargo.toml":

```Rust
[dependencies]
reqwest = { version = "0.10.6", features = ["blocking", "json"] }
```

Em seguida, importamos a biblioteca no nosso código:

```Rust
use reqwest;
```

Vamos enviar uma solicitação GET para a API do GitHub para obter os detalhes do nosso repositório. Para isso, precisamos fornecer a URL da API e a chave de acesso pessoal para autenticação:

```Rust
let url = "https://api.github.com/repos/<seu_nome_de_usuário>/<seu_repositório>";
let token = "<sua_chave_de_acesso_pessoal_github>";
```

Em seguida, criamos uma instância da solicitação e adicionamos o cabeçalho de autenticação com a chave de acesso pessoal:

```Rust
let client = reqwest::blocking::Client::new();
let res = client.get(url)
                .header("Authorization", format!("Bearer {}", token))
                .send();
```

Finalmente, imprimimos os dados de resposta no terminal:

```Rust
match res {
    Ok(res) => println!("Status: {}", res.status()),
    Err(err) => println!("Erro: {}", err),
}
```

## Deep Dive

Ao enviar uma solicitação HTTP em Rust, é importante entender os diferentes tipos de solicitações e respostas, bem como os códigos de status e cabeçalhos que podem ser retornados. A biblioteca "reqwest" suporta vários tipos de solicitações, incluindo GET, POST, PUT, DELETE, entre outros.

Além disso, é importante lembrar que as solicitações podem ser síncronas ou assíncronas em Rust. Na nossa solução acima, utilizamos uma solicitação síncrona com a função "send()", que bloqueia a thread atual até que a solicitação seja concluída. No entanto, também é possível utilizar solicitações assíncronas com a função "async()", que retorna uma promise a ser resolvida posteriormente.

Ao lidar com respostas, é importante entender os diferentes códigos de status, que indicam se a solicitação foi bem sucedida ou não, e os cabeçalhos, que contêm informações adicionais sobre a resposta, como o tipo de conteúdo retornado.

## Veja também

- Documentação da biblioteca "reqwest": https://docs.rs/reqwest/ 
- Tutorial sobre como realizar solicitações HTTP em Rust: https://www.freecodecamp.org/news/a-guide-to-making-http-requests-in-rust/

O envio de solicitações HTTP é uma habilidade essencial para qualquer programador Rust, e com as bibliotecas e recursos disponíveis, isso pode ser feito de forma clara e concisa. Esperamos que este artigo tenha sido útil para você aprender como realizar esta tarefa em Rust.
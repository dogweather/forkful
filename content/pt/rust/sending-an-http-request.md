---
title:                "Enviando uma solicitação http"
html_title:           "Rust: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que

Você já se perguntou como as aplicações web funcionam? Neste artigo, vamos explorar o envio de solicitações HTTP usando Rust. Isso pode ser útil para criar APIs, interagir com serviços web ou até mesmo construir seu próprio navegador!

## Como fazer

Para enviar uma solicitação HTTP em Rust, primeiro precisamos adicionar a dependência "reqwest" ao nosso projeto. Em seguida, podemos criar um cliente HTTP e usar o método `get()` para especificar a URL que queremos acessar.

```Rust
extern crate reqwest; // adicionando a dependência

fn main() {
    let client = reqwest::Client::new(); // criando um cliente HTTP
    let response = client.get("https://exemplo.com").send(); // enviando a solicitação

    match response {
        Ok(mut res) => { // caso a resposta seja bem-sucedida
            println!("Status code: {}", res.status()); // imprimindo o status da resposta
            let body = res.text().unwrap(); // extraindo o corpo da resposta e imprimindo
            println!("Resposta do servidor: {}", body);
        }
        Err(e) => println!("Erro: {}", e), // caso ocorra algum erro
    }
}
```

A saída deste código seria algo parecido com isso:

```
Status code: 200
Resposta do servidor: <!DOCTYPE html>
<html>
<head>
...
```

## Mergulho profundo

Ao usar o método `get()`, podemos fornecer uma série de opções para personalizar nossa solicitação. Algumas delas incluem o cabeçalho, corpo e autenticação. Podemos até mesmo enviar solicitações assíncronas usando o Rust Async/Await. Além disso, o pacote "reqwest" também suporta recursos de streaming e redirecionamento de solicitações.

## Veja também

Se você quiser aprender mais sobre como enviar solicitações HTTP com Rust, aqui estão alguns recursos úteis:

- [Documentação oficial do "reqwest"](https://docs.rs/reqwest/0.11.3/reqwest/)
- [Exemplos de código do "reqwest" no GitHub](https://github.com/seanmonstar/reqwest/tree/master/examples)
- [Tutorial de Rust sobre enviar solicitações HTTP](https://www.datbruce.com/2019/09/how-to-send-http-requests-in-rust.html)

Agora você está pronto para começar a enviar suas próprias solicitações HTTP em seus projetos Rust! Divirta-se explorando e criando incríveis aplicações web.
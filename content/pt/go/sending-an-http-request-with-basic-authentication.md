---
title:                "Enviando uma requisição http com autenticação básica"
html_title:           "Go: Enviando uma requisição http com autenticação básica"
simple_title:         "Enviando uma requisição http com autenticação básica"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes, é necessário enviar uma solicitação HTTP com autenticação básica para acessar recursos protegidos em um servidor. Isso pode ser feito facilmente usando o Go e suas bibliotecas nativas.

## Como Fazer

Para enviar uma solicitação HTTP com autenticação básica em Go, siga os seguintes passos:

1. Importe a biblioteca `net/http` para fazer solicitações HTTP.
2. Crie uma estrutura para armazenar as informações de autenticação básica, que é composta por um nome de usuário e uma senha.
3. Crie um cliente HTTP usando a função `NewRequest` do pacote `http` e defina o método HTTP como `GET`.
4. Defina o cabeçalho `Authorization` na solicitação com o valor `Basic <username>:<password>` codificado em base64.
5. Use a função `Do` do cliente HTTP para enviar a solicitação e receber uma resposta.
6. Verifique se a resposta tem um status de sucesso (código de status `200`).
7. Se tudo estiver correto, leia o corpo da resposta para ter acesso aos dados desejados.

Veja um exemplo de código completo:

```Go
import (
    "encoding/base64"
    "net/http"
)

type BasicAuth struct {
    Username string
    Password string
}

func main() {
    // Informações de autenticação.
    auth := BasicAuth{
        Username: "usuario",
        Password: "senha",
    }

    // Criando cliente HTTP.
    client := &http.Client{}

    // Criando solicitação GET.
    req, err := http.NewRequest("GET", "https://example.com", nil)
    if err != nil {
        panic(err)
    }

    // Definindo cabeçalho de autorização.
    basicAuth := "Basic " + base64.StdEncoding.EncodeToString([]byte(auth.Username+":"+auth.Password))
    req.Header.Add("Authorization", basicAuth)

    // Enviando solicitação e recebendo resposta.
    resp, err := client.Do(req)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    // Verificando status de sucesso.
    if resp.StatusCode == http.StatusOK {
        // Lendo corpo da resposta.
        // ...
    }
}
```

## Mergulho Profundo

Ao enviar uma solicitação HTTP com autenticação básica, é importante entender como o processo de autenticação funciona. O cabeçalho `Authorization` é utilizado para fornecer as credenciais de autenticação ao servidor. Ele é composto pelo método de autenticação (no caso, `Basic`) seguido por um espaço e depois pelas credenciais codificadas em base64.

A codificação em base64 é usada para converter as credenciais em uma sequência de caracteres que possa ser transmitida pelo cabeçalho HTTP sem causar problemas de formatação ou caracteres inválidos. No lado do servidor, as credenciais são extraídas do cabeçalho e verificadas para validar o acesso ao recurso protegido.

É importante ressaltar que o uso de autenticação básica não garante a segurança da comunicação entre cliente e servidor, pois as credenciais são transmitidas na forma de texto simples. Portanto, é recomendável utilizar métodos de autenticação mais seguros, como o HTTPS.

## Veja Também

- Documentação oficial do pacote `net/http`: https://golang.org/pkg/net/http/
- Página de referência sobre autenticação básica na especificação HTTP: https://tools.ietf.org/html/rfc7617
- Exemplos de autenticação básica em Go: https://gist.github.com/cuonghc/5123624
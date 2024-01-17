---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Go: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# O que e por que?

Enviar uma solicitação HTTP com autenticação básica é um processo em que um programador fornece credenciais de autenticação em uma solicitação HTTP para acessar um recurso protegido. Isso é necessário quando se precisa acessar uma API ou fazer uma requisição a um servidor que requer autenticação. É uma forma segura e eficiente de garantir que apenas usuários autorizados tenham acesso ao recurso.

# Como fazer:

Um exemplo simples de como enviar uma solicitação HTTP com autenticação básica em Go seria o seguinte:

```Go
// Importa o pacote "net/http" para realizar solicitações HTTP
import "net/http"

// Define as credenciais de autenticação
username := "meu_usuario"
password := "minha_senha"

// Cria o cliente HTTP com as credenciais
client := &http.Client {
  Transport: &http.Transport {
    // Configura as credenciais básicas de autenticação
    Proxy: http.ProxyFromEnvironment,
  },
}

// Cria uma solicitação GET para um recurso protegido
req, err := http.NewRequest("GET", "https://minhaapi.com/recurso", nil)
if err != nil {
  // Trata qualquer erro ocorrido
  fmt.Println("Erro ao criar a solicitação: ", err)
}

// Adiciona as credenciais à solicitação
req.SetBasicAuth(username, password)

// Envia a solicitação para o servidor e obtém a resposta
resp, err := client.Do(req)
if err != nil {
  // Trata qualquer erro ocorrido
  fmt.Println("Erro ao enviar a solicitação: ", err)
}

// Lê o corpo da resposta
body, err := ioutil.ReadAll(resp.Body)
if err != nil {
  // Trata qualquer erro ocorrido
  fmt.Println("Erro ao ler o corpo da resposta: ", err)
}

// Imprime o corpo da resposta
fmt.Println(string(body))

// Fecha o corpo da resposta
defer resp.Body.Close()
```

A saída do código acima seria algo como:

```
{
  "message": "Acesso autorizado ao recurso!"
}
```

# Profundando:

A autenticação básica é apenas uma das várias formas de autenticação em uma solicitação HTTP. Ela foi criada nos anos 90 como uma solução simples e eficiente para proteger recursos em servidores HTTP. Hoje, existem outras formas mais seguras de autenticação, como OAuth e token-based authentication. No entanto, a autenticação básica ainda é amplamente utilizada e suportada em diversas linguagens de programação, incluindo Go. Ao enviar uma solicitação com autenticação básica, os dados de autenticação são codificados em Base64, o que não é considerado uma medida de segurança forte. Por isso, é importante considerar alternativas mais seguras para proteger recursos protegidos.

# Veja também:

- [Documentação oficial do pacote "net/http" em Go](https://golang.org/pkg/net/http/)
- [Artigo sobre autenticação HTTP em Go](https://www.alexedwards.net/blog/http-authentication-in-golang)
- [Mais informações sobre autenticação básica em HTTP](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication)
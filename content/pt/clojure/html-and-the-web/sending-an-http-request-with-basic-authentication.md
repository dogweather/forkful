---
aliases:
- /pt/clojure/sending-an-http-request-with-basic-authentication/
date: 2024-01-20 18:01:20.935594-07:00
description: "Enviar uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica \xE9\
  \ o processo de acessar uma URL que exige um nome de usu\xE1rio e senha para entrar.\
  \ Programadores fazem\u2026"
lastmod: 2024-02-18 23:08:57.802433
model: gpt-4-1106-preview
summary: "Enviar uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica \xE9 o\
  \ processo de acessar uma URL que exige um nome de usu\xE1rio e senha para entrar.\
  \ Programadores fazem\u2026"
title: "Enviando uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Enviar uma requisição HTTP com autenticação básica é o processo de acessar uma URL que exige um nome de usuário e senha para entrar. Programadores fazem isso para interagir com APIs ou serviços web que requerem autenticação para garantir segurança e controle de acesso.

## Como Fazer:
```clojure
(require '[clj-http.client :as client])

(defn fetch-protected-resource [url username password]
  (let [credentials (str username ":" password)
        encoded-credentials (clojure.data.codec.base64/encode (.getBytes credentials))]
    (client/get url {:headers {"Authorization" (str "Basic " encoded-credentials)}})))

;; Exemplo de uso:
(println (fetch-protected-resource "https://api.exemplo.com/dados" "seu-usuario" "sua-senha"))
```
A função `fetch-protected-resource` recebe uma URL, um nome de usuário e uma senha, constrói o cabeçalho de autorização necessário e faz a requisição HTTP GET usando `clj-http.client`.

## Mergulho Profundo:
A autenticação básica HTTP é um mecanismo antigo e simples, parte do HTTP desde o HTTP/1.0. Envolve codificar o nome de usuário e a senha em Base64 e passá-los no cabeçalho da requisição. Apesar de fácil, não é a forma mais segura, já que as credenciais podem ser facilmente decodificadas se interceptadas. Por isso, deve ser usada apenas com HTTPS, que adiciona uma camada de criptografia.

Alternativas mais seguras incluem tokens de autenticação, como OAuth, que oferecem uma maneira mais robusta de controlar acessos sem expor diretamente as credenciais do usuário.

Quando implementar a autenticação básica em Clojure, a biblioteca `clj-http` é uma escolha comum. Ela abstrai muitos dos detalhes baixo-nível de fazer requisições HTTP e permite que você foque na lógica do seu programa. A codificação Base64 das credenciais pode ser feita usando a biblioteca `clojure.data.codec`.

## Veja Também:
- Documentação da clj-http: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- OAuth 2.0: [https://oauth.net/2/](https://oauth.net/2/)

---
date: 2024-01-20 18:01:20.935594-07:00
description: "Como Fazer: A fun\xE7\xE3o `fetch-protected-resource` recebe uma URL,\
  \ um nome de usu\xE1rio e uma senha, constr\xF3i o cabe\xE7alho de autoriza\xE7\xE3\
  o necess\xE1rio e faz a\u2026"
lastmod: '2024-04-05T21:53:46.520105-06:00'
model: gpt-4-1106-preview
summary: "A fun\xE7\xE3o `fetch-protected-resource` recebe uma URL, um nome de usu\xE1\
  rio e uma senha, constr\xF3i o cabe\xE7alho de autoriza\xE7\xE3o necess\xE1rio e\
  \ faz a requisi\xE7\xE3o HTTP GET usando `clj-http.client`."
title: "Enviando uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica"
weight: 45
---

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

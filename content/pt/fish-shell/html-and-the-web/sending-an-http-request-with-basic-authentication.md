---
title:                "Enviando uma requisição HTTP com autenticação básica"
aliases:
- /pt/fish-shell/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:42.714322-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando uma requisição HTTP com autenticação básica"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
Enviar uma requisição HTTP com autenticação básica significa incluir credenciais de username e password no cabeçalho da requisição. Programadores fazem isso para acessar recursos protegidos em servidores web.

## How to:
### Enviando uma requisição com autenticação básica usando `curl`:

```Fish Shell
set user "seu_usuario"
set pass "sua_senha"

# Codifica as credenciais em base64
set credentials (echo -n "$user:$pass" | base64)

# Faz a requisição com o cabeçalho de Autorização
curl -H "Authorization: Basic $credentials" [URL_do_Recurso]

# Comando alternativo que já inclui o cabeçalho de autenticação
curl -u "$user:$pass" [URL_do_Recurso]
```

### Exemplo de saída esperada:

```plaintext
{
  "data": "Informações protegidas"
}
```

## Deep Dive
A autenticação básica HTTP existe desde o início dos protocolos web, mas é simples e não tão segura quanto métodos modernos, como tokens e OAuth, pois transmite credenciais em texto claro codificado em base64, que pode ser facilmente decodificado. Alternativas incluem JWT (JSON Web Tokens) ou autenticação Digest, que oferecem maior segurança. Quando se usa a autenticação básica, é importante que a comunicação seja feita através de HTTPS para adicionar uma camada de segurança com criptografia SSL/TLS.

Detalhes sobre a implementação:

1. Codificar as credenciais em base64 não as criptografa, apenas as codifica para transmissão.
2. O cabeçalho de autorização `Authorization: Basic [credenciais]` é o que o servidor utiliza para validar a autenticação.
3. A ferramenta `curl` é poderosa e versátil para fazer requisições HTTP direto do terminal.
   
## See Also
- Documentação oficial do `curl`: https://curl.se/docs/manpage.html
- Autenticação HTTP na MDN Web Docs: https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication
- Segurança em autenticação HTTP: https://owasp.org/www-community/attacks/Basic_access_authentication

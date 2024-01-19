---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Enviando uma Solicitação HTTP com Autenticação Básica no Fish Shell

## O que é e por quê?

O envio de uma solicitação HTTP com autenticação básica é quando você faz uma requisição para um servidor web com um nome de usuário e senha para obter acesso a um recurso protegido. É frequentemente usado por programadores para acessar APIs ou outras áreas protegidas de um site.

## Como Fazer:

No Fish Shell, você pode usar o comando `curl` para enviar essa requisição. Aqui está um exemplo básico:

```Fish Shell
set user 'usuario'
set pass 'senha'
curl -u $user:$pass http://exemplo.com
```

E aqui está como parece a saída:

```Fish Shell
<html>
   <head>
      <title>Exemplo</title>
   </head>
   <body>
      Bem-vindo ao exemplo.com!
   </body>
</html>
```

## Mergulho Profundo

O Fish Shell foi criado em 2005 como uma alternativa mais amigável e ponderada para os shells UNIX tradicionais. Ele tem uma sintaxe simples e clara que o torna ideal para tarefas de scripting como enviar uma solicitação HTTP.

Quanto às alternativas para o envio de solicitações HTTP no Fish Shell, você também pode usar o `wget`. O `wget` é uma ferramenta de linha de comando não interativa para recuperar arquivos da web.

```Fish Shell
wget --user=$user --password=$pass http://exemplo.com
```

A formação da autenticação básica HTTP no Fish Shell, é assim; primeiramente, a string "$user:$pass" é codificada em Base64. Em seguida, essa string codificada é incluída no cabeçalho da requisição HTTP. O servidor, então, decodifica a string para validar o nome de usuário e a senha.

## Veja também

Para mais informações sobre como enviar solicitações HTTP com autenticação básica, dê uma olhada nos seguintes recursos:

- Documentação do Fish Shell: https://fishshell.com/docs/current/index.html
- Documentação do cURL: https://curl.haxx.se/docs/manpage.html
- Página da Wikipédia sobre Autenticação básica de acesso: https://pt.wikipedia.org/wiki/Autentica%C3%A7%C3%A3o_b%C3%A1sica_de_acesso
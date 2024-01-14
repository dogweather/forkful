---
title:                "Fish Shell: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que usar a Fish Shell para enviar uma solicitação HTTP com autenticação básica?

Enviar solicitações HTTP é uma tarefa comum em programação, e a autenticação básica é um meio simples de garantir a segurança das informações transmitidas através dessas solicitações. A Fish Shell tem uma sintaxe simplificada e intuitiva, o que a torna uma ótima escolha para automatizar esse processo.

## Como fazer:

Para demonstrar como enviar uma solicitação HTTP com autenticação básica usando a Fish Shell, seguimos os seguintes passos:

1. Instale a Fish Shell em seu sistema, caso ainda não tenha.
2. Crie um novo arquivo de script com a extensão `.fish` e adicione as seguintes linhas de código:

```Fish Shell
set -gx username "seuusername" # substitua pelo seu nome de usuário
set -gx password "suasenha" # substitua pela sua senha

set headers -r "Content-Type: application/json" # define o cabeçalho da solicitação

set data -r "{ \"key\": \"value\" }" # define os dados a serem enviados na solicitação

curl -u $username:$password -H $headers -d $data https://api.example.com/endpoint # envia a solicitação com a autenticação básica e dados definidos
```

3. Salve o arquivo e execute-o no terminal usando `fish nome-do-arquivo.fish`.
4. Se a solicitação for bem sucedida, você deve receber a resposta no formato JSON contendo a chave e o valor definidos no passo 2.

## Profundando no assunto:

No código acima, usamos o comando `set -r` para definir variáveis que serão usadas durante o envio da solicitação. A opção `-r` garante que a string definida não será interpretada, o que pode ser importante para evitar problemas com caracteres especiais nas suas credenciais ou dados da solicitação.

Outro ponto a ser observado é o uso do comando `curl`, que é uma ferramenta de linha de comando para transferir dados usando vários protocolos, incluindo o HTTP. Neste caso, utilizamos a opção `-u` para definir o nome de usuário e senha para a autenticação básica. Também definimos o cabeçalho da solicitação e os dados a serem enviados usando as variáveis criadas no passo 2.

Lembrando que essas são apenas algumas opções de utilização da Fish Shell para enviar uma solicitação HTTP com autenticação básica. Você pode explorar mais comandos e opções para personalizar o processo de acordo com suas necessidades.

## Veja também:

- [Documentação da Fish Shell](https://fishshell.com/docs/current/index.html)
- [Curl Command Tutorial](https://www.codecademy.com/articles/command-line-curl)
- [API Design Guide for HTTP APIs](https://docs.microsoft.com/en-us/azure/architecture/best-practices/api-design)
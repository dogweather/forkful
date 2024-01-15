---
title:                "Escrevendo para o erro padrão"
html_title:           "Bash: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que

Você pode estar se perguntando por que escrever para o erro padrão em seu script Bash? Bem, a resposta é simples - para lidar melhor com erros e depuração. Ao escrever mensagens de erro para o erro padrão, você pode facilmente encontrar e corrigir problemas em seu código.

## Como Fazer

Para escrever para o erro padrão em Bash, você pode usar o comando `echo` com a opção `-e` para habilitar escapes de caractere. Aqui está um exemplo de código para imprimir uma mensagem de erro:

```Bash
echo -e "Erro: Não foi possível encontrar o arquivo."
```

Isso irá imprimir a mensagem na tela do erro padrão. Você também pode redirecionar o erro padrão para um arquivo usando o operador `>` após o comando `echo`. Por exemplo:

```Bash
echo "Erro: Não foi possível encontrar o arquivo." > erro.log
```

Isso irá redirecionar a mensagem de erro para um arquivo chamado "erro.log".

## Aprofundando

Para uma opção mais avançada, você pode usar o comando `>&2` após o comando `echo` para redirecionar explicitamente a saída para o erro padrão. Isso é útil quando você precisa escrever mensagens de erro dentro de uma função em seu script Bash. Aqui está um exemplo de código:

```Bash
funcao_erro() {
    >&2 echo "Erro: Nome de usuário inválido."
}

funcao_erro
```

Este código irá enviar a mensagem de erro para o erro padrão, mesmo que a função esteja sendo chamada dentro do script.

## Veja Também

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/)
- [Artigo sobre redirecionamento de entrada/saída em Bash](https://www.digitalocean.com/community/tutorials/an-introduction-to-linux-i-o-redirection)
- [Lista de comandos Bash úteis](https://cheatography.com/davechild/cheat-sheets/linux-command-line/)
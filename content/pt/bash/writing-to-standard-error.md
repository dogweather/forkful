---
title:                "Bash: Escrevendo para o erro padrão"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão, também conhecido como stderr, é uma técnica importante na programação Bash. Quando um programa é executado, ele pode gerar mensagens de erro e aviso. Ao escrevê-las para o erro padrão, essas mensagens podem ser facilmente identificadas e tratadas pelo usuário.

## Como fazer?

Para escrever mensagens para o erro padrão em Bash, usamos o comando `>&2`, seguido pela mensagem que desejamos exibir. Por exemplo, no seguinte código, estamos tentando excluir um arquivo que não existe e escrevemos a mensagem de erro para o erro padrão:

```Bash
rm arquivo_que_nao_existe >&2
```

O usuário verá a seguinte saída:

```Bash
rm: não foi possível remover 'arquivo_que_nao_existe': Arquivo ou diretório não encontrado
```

Podemos usar essa técnica para fornecer informações úteis ao usuário, como instruções para corrigir o erro, ou até mesmo informar sobre o status de execução do programa.

## Mergulho profundo

Quando escrevemos para o erro padrão, podemos especificar o número do descritor de arquivo para onde a mensagem será redirecionada. Por padrão, o descritor `2` é usado, mas podemos alterar isso para enviar a mensagem para outros locais. Isso é especialmente útil quando estamos lidando com a saída de diferentes comandos e queremos que as mensagens de erro sejam redirecionadas para locais específicos.

Outra dica importante é usar o comando `exit` em conjunto com o redirecionamento para o erro padrão. Ao receber uma mensagem de erro, podemos usar o comando `exit 1` para sair do programa imediatamente. Isso é útil porque geralmente queremos que o programa pare de ser executado se encontrarmos um erro.

## Veja também

- [Redirecionamento de saídas em Bash](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)
- [Como e por que redirecionar saída de erros no Bash?](https://www.digitalocean.com/community/tutorials/how-and-why-to-redirect-stderr-and-stdout-in-bash)
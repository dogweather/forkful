---
title:    "Fish Shell: Escrevendo para o erro padrão."
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão

Escrever para o erro padrão, também conhecido como "stderr", é uma parte importante do processo de programação. Ao direcionar mensagens de erro para o erro padrão, podemos identificar e corrigir possíveis problemas em nosso código, tornando-o mais preciso e confiável.

## Como fazer

Para escrever para o erro padrão no Fish Shell, podemos usar o comando `echo` seguido do operador `>`:

```Fish Shell
echo "Este é um exemplo de mensagem de erro" > stderr
```

Podemos então verificar o conteúdo do arquivo "stderr", que deve conter a mensagem que acabamos de escrever:

```Fish Shell
cat stderr
```

A saída seria:

```
Este é um exemplo de mensagem de erro
```

Também podemos redirecionar as mensagens de erro diretamente ao escrever o comando, usando o operador `2>`:

```Fish Shell
comando_inexistente 2> stderr
```

Isso irá redirecionar as mensagens de erro geradas pelo comando inexistente para o arquivo "stderr", permitindo-nos identificar e resolver possíveis problemas.

## Mergulho Profundo

Ao escrever para o erro padrão, é importante ter em mente que ele é utilizado principalmente para mensagens de erro e não deve ser usado para saídas regulares do programa. Além disso, é possível direcionar as mensagens de erro para outros lugares, como para um arquivo de log, se necessário.

Vale ressaltar também que, no Fish Shell, podemos usar o comando `&>` para redirecionar tanto as saídas quanto as mensagens de erro para um mesmo destino. Isso pode ser útil em situações em que queremos salvar todo o registro de execução de um determinado comando.

## Veja também

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guia básico para iniciantes em Fish Shell](https://dev.to/dusanpucik/getting-started-with-fish-shell-a-simple-guide-for-beginners-2o00)
- [Como escrever scripts no Fish Shell](https://www.shell-tips.com/pt-br/2010/08/02/how-to-write-a-shell-script-in-fish-shell/)
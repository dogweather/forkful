---
title:                "Escrevendo para o erro padrão"
html_title:           "Fish Shell: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que

Se você está programando em Fish Shell, é possível que precise escrever para saída de erro padrão (standard error, em inglês) em algumas situações. Isso pode ser útil para lidar com erros no seu código, melhorar a depuração e fazer logs mais precisos.

## Como Fazer

Escrever para saída de erro padrão em Fish Shell é bastante simples. Você pode usar o comando `echo` seguido do sinal de maior (`>`) e o número 2 para especificar a saída de erro padrão. Veja o exemplo abaixo:

```
Fish Shell
echo "Erro! Algo deu errado" >[2]
```

Isso irá escrever a mensagem "Erro! Algo deu errado" na saída de erro padrão. Você também pode redirecionar a saída de erro padrão para um arquivo usando o mesmo comando, com o sinal de maior seguido do nome do arquivo em que você deseja salvar a mensagem de erro.

## Mergulho Profundo (Deep Dive)

Ao escrever para saída de erro padrão, é importante entender como o Fish Shell lida com isso para evitar erros e obter o resultado desejado. Ao usar o `echo` com o sinal de maior, você está direcionando a mensagem para a saída de erro padrão, que é tratada separadamente da saída padrão. Isso garante que os erros sejam exibidos corretamente e não se misturem com a saída padrão do seu script.

Outro ponto importante é entender a diferença entre os números de saída (`1` e `2`). O número `1` representa a saída padrão, enquanto o número `2` representa a saída de erro padrão. Certifique-se de usar o número correto para direcionar a mensagem de erro para a saída correta.

## Veja também

- [Documentação do Fish Shell](https://fishshell.com/docs/current/) - Leia mais sobre a sintaxe e recursos do Fish Shell.
- [Writing to Standard Error in Bash](https://www.linuxjournal.com/content/writing-standard-error) - Artigo sobre como escrever para saída de erro padrão em Bash.
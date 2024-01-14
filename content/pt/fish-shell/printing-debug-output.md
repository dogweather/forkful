---
title:                "Fish Shell: Exibindo saída de depuração"
simple_title:         "Exibindo saída de depuração"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que
Printar saída de debug é uma ferramenta essencial para aqueles que programam em Fish Shell. Isso permite que você acompanhe o fluxo do seu código e identifique possíveis erros ou problemas de forma mais eficiente.

## Como fazer
Para imprimir uma saída de debug em Fish Shell, você pode usar o comando `echo`. Por exemplo:

```Fish Shell
echo "Debug message"
```
Isso imprimirá a mensagem "Debug message" na saída padrão. Você também pode usar variáveis ou comandos dentro do comando `echo` para imprimir informações específicas, como no exemplo abaixo:

```Fish Shell
echo "Variável X: $x"
echo "Data atual: (date)"
```

Além disso, você pode redirecionar a saída de debug para um arquivo para facilitar a organização e visualização dos resultados. Para isso, basta usar o operador `>` seguido do nome do arquivo desejado. Por exemplo:

```Fish Shell
echo "Debug output" > debug.log
```

## Deep Dive
Além do comando `echo`, o Fish Shell também possui outras opções para imprimir saída de debug. Um deles é o comando `printf`, que permite uma formatação mais complexa da saída. Por exemplo:

```Fish Shell
set x "variável"
printf "O valor da %s é %d" $x $x
```

Outra opção é usar o comando `fish_print`, que também permite uma formatação mais avançada e a impressão de cores e estilos na saída. Você pode encontrar mais informações sobre esses comandos e suas opções na documentação oficial do Fish Shell.

## Veja também
- Documentação oficial do Fish Shell: [https://fishshell.com/docs/current/](https://fishshell.com/docs/current/)
- Tutorial sobre `echo` e `printf` em Fish Shell: [https://hackertarget.com/linux-unix-printf-command-tutorial/](https://hackertarget.com/linux-unix-printf-command-tutorial/)
- Tutorial avançado sobre saída de debug em Fish Shell: [https://ss64.com/fish/echo.html](https://ss64.com/fish/echo.html)
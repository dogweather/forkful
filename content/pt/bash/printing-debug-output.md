---
title:    "Bash: Imprimindo saída de depuração"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Você já se deparou com um código que simplesmente não estava funcionando e se perguntou onde estava o erro? Ou talvez você tenha feito várias alterações em uma parte do seu programa e não conseguiu identificar qual alteração causou o problema? Bem, é aí que o debug output se torna útil. Ele permite que você imprima informações durante a execução do seu código, facilitando a identificação de erros e problemas.

## Como fazer

Para imprimir um debug output em Bash, você pode utilizar o comando `echo` seguido da informação que deseja imprimir. Por exemplo:

```Bash
echo "Variável x = $x"
```

Você também pode utilizar o redirecionamento de saída (`>` ou `>>`) para gravar o output em um arquivo:

```Bash
echo "Erro: não foi possível abrir o arquivo" >> erros.log
```

Outra opção é utilizar o comando `printf` para formatar a saída:

```Bash
printf "O valor de x é %.2f" $x
```

## Deep Dive

Além dos exemplos mencionados acima, existem outras maneiras de imprimir debug output em Bash. Por exemplo, você pode utilizar o comando `set -x` para ativar o modo de debug, que exibirá todas as linhas do seu código antes de serem executadas. Este modo pode ser desativado utilizando `set +x`.

Também é possível criar funções personalizadas para imprimir debug output. Isso pode ser útil para imprimir informações específicas em determinados pontos do seu código. Por exemplo:

```Bash
debug() {
   if [ "$DEBUG" == "true" ]; then
      echo "$@"
   fi
}

# Utilizando a função
debug "Variável y = $y"
```

## Veja também

- [Guia de referência rápida do Bash](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)
- [Documentação do comando echo](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Bash-Builtins)
- [Artigo sobre debugging em Bash](https://bash-prompt.net/guides/debugging-bash-scripts/)
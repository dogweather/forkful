---
title:    "Bash: Colocando em maiúscula uma string"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string?

Capitalizar uma string é um processo comum em programação, especialmente em Bash. Isso significa transformar todas as letras de uma string em maiúsculas ou minúsculas, dependendo do que é necessário. Existem várias razões pelas quais alguém pode querer capitalizar uma string em um programa Bash.

## Como fazer isso:

Para capitalizar uma string em Bash, você pode usar o comando `tr` (traduzir) junto com algumas opções. O `tr` é um utilitário que funciona substituindo ou excluindo caracteres em um fluxo de texto. Para capitalizar uma string, podemos usar as opções `-u` (para maiúsculas) e `-t` (para aplicar as alterações apenas nos caracteres alfabéticos). Aqui está um exemplo de como ficaria o código:

```Bash
texto="exemplo"
texto_capitalizado=$(echo $texto | tr '[:lower:]' '[:upper:]')
echo $texto_capitalizado
```

A saída seria "EXEMPLO", com todas as letras maiúsculas. Você também pode usar a opção `-d` para excluir caracteres, por exemplo, para remover todos os espaços em branco de uma string antes de capitalizá-la.

## Mergulho Profundo:

Além do `tr`, existem outras maneiras de capitalizar uma string em Bash. Você também pode usar o comando `awk` com a função `toupper`, que transforma todos os caracteres em maiúsculas. Ou, se a sua string contiver acentos, você pode usar o utilitário `sed` junto com expressões regulares para capitalizar corretamente.

Além disso, é importante observar que em Bash, as variáveis são case sensitive, ou seja, "texto" é diferente de "TEXTO". Portanto, ao capitalizar uma string, verifique se você está usando os mesmos caracteres em todas as instâncias da variável.

## Veja também:

- [Artigo sobre utilização do comando `tr` em Bash](https://tecadmin.net/learn-tr-command-with-15-practical-examples/)
- [Tutorial do Awk para iniciantes](https://www.grymoire.com/Unix/Awk.html)
- [Documentação oficial do Sed](https://www.gnu.org/software/sed/manual/sed.html)

Código do exemplo baseado em: https://www.cyberciti.biz/faq/bash-convert-string-into-uppercase/
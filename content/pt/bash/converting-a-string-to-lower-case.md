---
title:    "Bash: Convertendo uma string para letras minúsculas"
keywords: ["Bash"]
---

{{< edit_this_page >}}

Por que converter uma string para letras minúsculas?

Ao trabalhar com Bash, muitas vezes nos deparamos com a necessidade de converter uma string para letras minúsculas. Isso pode ser útil para padronizar a entrada do usuário, facilitar a comparação de strings ou simplesmente tornar a saída mais legível.

## Como fazer

A conversão de uma string para letras minúsculas em Bash é muito simples. Utilizamos o comando `tr` seguido da opção `-s`. Dentro das aspas simples, especificamos as letras maiúsculas que queremos substituir pelas minúsculas. Por exemplo:

```Bash
str="BASH Programação"
str=$(echo "$str" | tr '[:upper:]' '[:lower:]')
echo "$str"
```

A saída desta execução seria "bash programação", com todas as letras em minúsculas.

## Mergulho profundo

O comando `tr` utilizado no exemplo acima é realmente poderoso e oferece diversas opções de uso. Podemos, por exemplo, substituir não apenas letras, mas também números e caracteres especiais.

Além disso, é possível utilizar expressões regulares para especificar quais caracteres devem ser afetados pela conversão. Também é possível utilizar variáveis e pipes para combinar o comando `tr` com outras funcionalidades.

No entanto, é importante ter cuidado ao utilizar este comando, pois ele também pode alterar valores que não devem ser substituídos, como espaços em branco ou pontuação.

## Veja também

- Documentação oficial do comando `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Outras dicas úteis de Bash: https://www.geeksforgeeks.org/bash-scripting-important-tips/
- Aprenda Bash passo a passo: https://www.linuxtips.io/aprenda-shell-script-em-5-exemplos
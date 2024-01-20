---
title:                "Verificando se um diretório existe"
html_title:           "Fish Shell: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Verificar se um diretório existe é o ato de confirmar se um caminho específico do diretório está disponível em seu sistema de arquivos. Programadores fazem isso para evitar erros ao tentar acessar ou manipular diretórios que podem não existir.

## Como fazer:

Para verificar se um diretório existe no Fish Shell, você deve usar a função `test -d` seguida do caminho do diretório. Aqui está um exemplo:

```fish
if test -d /caminho/para/seu/diretorio
   echo "O diretório existe."
else
   echo "O diretório não existe."
end
```

A saída do script será "O diretório existe." se o caminho especificado existir, e "O diretório não existe." se não existir.

## Análise mais aprofundada

A necessidade de verificar a existência de um diretório remonta às primeiras formas de sistemas operacionais onde os erros de E/S dispendiosos poderiam ocorrer ao tentar acessar um diretório inexistente. No Fish Shell, a declaração `test -d` é nativa e muito eficiente, sendo uma abordagem preferida pela maioria dos programadores.

Existem alternativas ao `test -d`, como o uso do comando `ls`. No entanto, `ls` é menos direto e pode produzir erros de E/S. Além disso, estáveis e mais ineficientes se você só precisa verificar a existência do diretório:

```fish
if ls /caminho/para/seu/diretorio >/dev/null 2>&1
   echo "O diretório existe."
else
   echo "O diretório não existe."
end
```

Esta implementação envolve o redirecionamento do stdout e stderr para /dev/null, o que pode não ser ideal em todos os cenários. Portanto, `test -d` é geralmente a abordagem preferida.

## Veja também

- [Documentação oficial do Fish Shell sobre o comando `test`](https://fishshell.com/docs/current/cmds/test.html)
- [Tutorial completo sobre manipulação de arquivos e diretórios no Fish Shell](https://tutorial.djangogirls.org/pt/python_introduction/#o-que-e-a-linha-de-comando)
- [Dicas e truques do Fish Shell](https://www.sitepoint.com/tips-tricks-for-the-fish-shell/)
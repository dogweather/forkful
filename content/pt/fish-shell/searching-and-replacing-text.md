---
title:                "Fish Shell: Busca e substituição de texto"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que Utilizar a Shell de Peixe?

Muitas vezes, ao trabalhar com arquivos de texto, é necessário fazer mudanças rápidas e precisas nos conteúdos. A Shell de Peixe oferece uma maneira eficiente de realizar essa tarefa, economizando tempo e esforço. Neste artigo, vamos explorar como podemos utilizar a Shell de Peixe para buscar e substituir texto em nossos arquivos.

## Como Utilizar a Shell de Peixe para Buscar e Substituir Texto

```Fish Shell
# Buscando e substituindo texto no terminal
sed -i 's/antigo/novo/g' arquivo.txt
```

Neste exemplo, usamos o comando "sed" para buscar e substituir todas as ocorrências da palavra "antigo" pela palavra "novo" no arquivo chamado "arquivo.txt". A opção "-i" indica que queremos que as mudanças sejam feitas diretamente no arquivo, em vez de imprimir o resultado no terminal.

```Fish Shell
# Usando expressões regulares
sed -i 's/[0-9]\{3\}/XXX/g' arquivo.txt
```

Aqui, usamos uma expressão regular para buscar e substituir todos os números de três dígitos no arquivo por "XXX". Isso pode ser útil para ocultar informações sensíveis, como números de telefone ou CPFs.

## Profundidade no Uso de Busca e Substituição de Texto

A Shell de Peixe possui outras ferramentas além do comando "sed" que podem ser úteis para buscar e substituir texto. Por exemplo, o comando "grep" pode ser usado para buscar linhas de texto que correspondam a um padrão específico, enquanto o comando "awk" permite que você execute operações mais complexas em arquivos de texto.

Além disso, é possível criar scripts e funções personalizadas para automatizar tarefas de busca e substituição de texto em diferentes situações. Com um pouco de prática, a Shell de Peixe se torna uma ferramenta poderosa para manipulação de texto.

## Veja Também

- [Tudo o que você precisa saber sobre a Shell de Peixe](https://fishshell.com/)
- [Documentação oficial do comando "sed"](https://www.gnu.org/software/sed/manual/sed.html)
- [Guia de expressões regulares na Shell de Peixe](https://fishshell.com/docs/current/tutorial.html#tutorial-regex)
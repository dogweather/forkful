---
title:                "Usando expressões regulares"
html_title:           "Gleam: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O que & Por que?
Regular Expressions (expressões regulares) são sequências de caracteres que formam um padrão de pesquisa. Programadores usam expressões regulares para manipular texto de forma mais eficiente e simplificar processos de validação de strings.

## Como fazer:
Vamos dar uma olhada em alguns exemplos básicos usando o Fish Shell.

- Para verificar se uma string está dentro de outra string, você poderia usar o comando `string match`.

```Fish Shell
set minha_string "Olá, mundo!"
string match -r "mundo" $minha_string
```

Saída:
```
mundo
```

- Buscando todas as ocorrências de uma string específica em um arquivo usamos `grep`:

```Fish Shell
grep -o 'Mundo' meu_arquivo.txt
```

## Aprofundamento
(1) As expressões regulares têm sua origem na teoria formal de linguagem e automatos da ciência da computação. Elas se tornaram especialmente populares após serem incluídas no utilitário de edição de texto Unix, `ed`, em 1970.  
(2) Alternativas: Padrões de pesquisa semelhantes podem ser implementados também usando wildcards ou globbing, mas são menos poderosos e flexíveis em comparação com as expressões regulares. 
(3) Fish Shell usa uma implementação de expressões regulares baseada em POSIX, que é uma das implementações de expressões regulares mais comuns, bastante utilizada em Unix-like systems.

## Veja também
- Documentação oficial do Fish Shell: https://fishshell.com/docs/current/index.html
- Guia de expressões regulares: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions
- Para uma compreensão mais profunda da teoria por trás das expressões regulares, consulte este tutorial do MIT: https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-045j-automata-computability-and-complexity-spring-2011/lecture-notes/MIT6_045JS11_lec04.pdf.
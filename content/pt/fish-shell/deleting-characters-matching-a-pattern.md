---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Removendo caracteres que correspondem a um padrão no Fish Shell

## Por quê e para quê?

A remoção de caracteres correspondendo a um padrão é a tarefa de excluir um conjunto específico de caracteres dos dados de entrada. Os programadores fazem isso para limpar ou formatar dados, melhorando a eficiência da manipulação de strings e processos de análise.

## Como fazer:

Vamos usar o comando `string` do Fish Shell, especificamente `string match` e `string replace`. Aqui estão exemplos de como fazê-lo.

```Fish Shell
# Padrão de caracteres
set pattern "o"

# String de entrada
set input "Olá, mundo!"

# Correspondendo e removendo o padrão
set output (string replace -r -a $pattern "" $input)

echo $output
```
Na execução, o output vai ser: 

```Fish Shell
"lá, mund!"
```
Este código corresponde e remove todas as instâncias do padrão na string de entrada.

## Mergulho fundo

Historicamente, muitos shells do Unix, como o Bash, têm fornecido funções padrão para a correspondência e substituição de padrões. No entanto, o Fish Shell, introduzido em 2005, introduziu um design mais user-friendly e uma sintaxe de script mais simples, que incluía recursos poderosos de manipulação de strings.

Existem alternativas ao `string replace` para a remoção de caracteres, como o uso de ferramentas externas de comando como `sed` e `awk`. Essas ferramentas oferecem mais flexibilidade, mas podem ser mais complexas para novos programadores.

A implementação da remoção de caracteres que correspondem a um padrão no Fish Shell é bastante simples, mas eficaz. A opção `-r` no `string replace` habilita a substituição de padrões regex, permitindo maior flexibilidade na manipulação de strings.

## Veja também:

- Documentação oficial do Fish Shell: https://fishshell.com/docs/current/index.html
- Guia de introdução ao regex: https://regexr.com/
- Tutorial de manipulação de strings no Fish: https://fishshell.com/docs/current/tutorial.html#tut_strings
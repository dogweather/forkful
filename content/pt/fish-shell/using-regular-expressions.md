---
title:                "Utilizando expressões regulares"
date:                  2024-01-19
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que São & Por Que Usar?
Expressões regulares são padrões usados ​​para encontrar correspondências de texto de acordo com regras definidas. Programadores utilizam para pesquisar, substituir e manipular strings com eficiência e precisão.

## Como Fazer:
```Fish Shell
# Encontrar todos os arquivos .txt
ls *.txt

# Procurar pela palavra 'fish' em um arquivo
grep 'fish' arquivo.txt

# Substituir 'fish' por 'shark' em todos os arquivos .txt
sed -i 's/fish/shark/g' *.txt

# Exemplo de saída para o comando grep
arquivo.txt: Esta linha contém a palavra fish.
```

## Mergulho Profundo
Expressões regulares têm sua origem nos anos 1950, com linguagens formais e teoria dos autômatos. Alternativas incluem a busca simples por texto ou o uso de parsers para estruturas complexas. Na Fish Shell, os comandos `grep`, `sed` e `awk` são frequentemente utilizados para trabalhar com regex, mas a linguagem não possui um operador de regex embutido como em algumas outras shells.

## Veja Também
- Tutorial oficial do `grep`: https://www.gnu.org/software/grep/manual/grep.html
- Documentação do `sed`: https://www.gnu.org/software/sed/manual/sed.html
- Um guia online para testar regex: https://regexr.com/
- Documentação da Fish Shell: https://fishshell.com/docs/current/index.html

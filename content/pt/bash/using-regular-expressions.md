---
title:                "Utilizando expressões regulares"
date:                  2024-01-19
simple_title:         "Utilizando expressões regulares"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Expressões regulares são padrões usados para encontrar correspondências de textos. Programadores as utilizam para busca, substituição e análise de dados de forma eficiente e flexível.

## How to:
Para usar expressões regulares em Bash, podemos empregar comandos como `grep`, `sed`, e `awk`. Aqui estão alguns exemplos:

```Bash
# Encontrar linhas que contenham "casa":
echo -e "minha casa\nseu carro\nnossa casa" | grep 'casa'

# Substituir 'gato' por 'cachorro' em um texto:
echo "O gato subiu no telhado" | sed 's/gato/cachorro/'

# Filtrar linhas e mostrar apenas números de um arquivo:
cat numeros.txt | awk '/^[0-9]+$/'
```

Saída esperada pode ser algo como:
```
minha casa
nossa casa
O cachorro subiu no telhado
123
456
```

## Deep Dive
Expressões regulares surgiram nos anos 1950 com trabalhos teóricos do matemático Stephen Kleene. O `grep` foi um dos primeiros programas a implementar expressões regulares de forma prática. Alternativas a expressões regulares incluem parsers de sintaxe específica e algoritmos de busca de texto. Expressões regulares em Bash são geralmente mais básicas quando comparadas com as disponíveis em linguagens como Perl ou Python, mas ainda assim são poderosas para operações de linha de comando.

## See Also
- Documentação do GNU `grep`: https://www.gnu.org/software/grep/manual/grep.html
- Tutorial interativo de expressões regulares: https://regexone.com/
- `sed` & `awk` 101 Hacks: https://www.thegeekstuff.com/2010/01/awk-introduction-tutorial-7-awk-print-examples/

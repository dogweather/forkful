---
title:                "Extraindo substrings"
aliases:
- /pt/bash/extracting-substrings.md
date:                  2024-01-20T17:45:14.371612-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extraindo substrings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Extrair substrings significa pegar pedaços de uma string. Programadores fazem isso para manipular e analisar dados mais facilmente.

## Como fazer:
Vamos direto ao código. Suponha que você quer pegar um pedaço da string "Bash é demais!"

```Bash
# Exemplo 1: Pegando os primeiros 4 caracteres
string="Bash é demais!"
echo ${string:0:4} # Saída: Bash

# Exemplo 2: Ignorando os primeiros 5 caracteres e pegando os próximos 2
echo ${string:5:2} # Saída: é 

# Exemplo 3: Extraindo até o final da string, começando do caracter 10
echo ${string:10} # Saída: mais!
```

Cada exemplo mostra como pegar partes específicas da sua string. Altere os números para ajustar ao que precisa.

## Mergulho Profundo
Antigamente, extrair substrings em shells de Unix era um pouco engenhoso, usando ferramentas como `cut`, `awk` ou `sed`. A partir do Bash 2.0, essa funcionalidade foi embutida. Aqui uma olhada em algumas nuances:

- **Indexação começa do 0**: O primeiro caracter é 0, o segundo é 1, e assim por diante.
- **Negativos? Também dá**: Pode-se usar índices negativos para começar a contar do final da string (precisa do Bash versão 4.2+).
- **Tamanho é opcional**: Se omitir o tamanho, você pega tudo até o final.
- **Alternativas**: `awk`, `cut`, `grep`, `sed` ou até linguagens como Python ou Perl.

Detalhes de implementação:

- **Variáveis e parâmetros**: O Bash trata substrings dentro de variáveis usando a sua sintaxe `${variable:start:length}`.
- **Eficiência**: Extrair substrings diretamente no Bash é geralmente mais rápido já que não invoca processos externos como os comandos `awk` ou `sed`.

## Veja também
Confira estes links para se aprofundar ainda mais:

- [Bash String Manipulation Guide](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/string-manipulation.html)
- [Stack Overflow - Extracting strings in Bash](https://stackoverflow.com/questions/tagged/bash+string)

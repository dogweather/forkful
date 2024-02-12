---
title:                "Convertendo uma string para minúsculas"
aliases:
- pt/bash/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:37:49.120814-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma string para minúsculas"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Converter uma string para minúsculas é o processo de transformar todas as letras de uma string para a forma minúscula. Programadores fazem isso para padronizar dados, facilitar comparações de strings e garantir consistência em operações que são sensíveis a maiúsculas e minúsculas.

## How to:
```Bash
# Usando tr
echo "Converta ESSA STRING para minúsculas" | tr '[:upper:]' '[:lower:]'

# Usando a construção ${variável,,}
minha_string="MAIÚSCULAS para minúsculas"
echo ${minha_string,,}

# Com awk
echo "Outra STRING em Maiúsculas" | awk '{print tolower($0)}'
```
Saída de exemplo para os comandos acima:
```
converta essa string para minúsculas
maiúsculas para minúsculas
outra string em maiúsculas
```

## Deep Dive

A necessidade de converter strings para minúsculas remonta ao início da computação, onde a consistência na entrada de dados era crítica. No tempo dos primeiros computadores, a capacidade de processamento e memória era limitada, então a normalização de strings para um formato padrão economizava recursos valiosos.

No UNIX e em seus derivados, como o Linux, ferramentas como `tr`, `awk`, e as capacidades internas do Bash, como operadores de substituição de padrões, oferecem diferentes formas de realizar a conversão para minúsculas.

- `tr` é um comando padrão do UNIX que significa "translate" ou "transliterate". É simples e rápido para transformações de personagens.

- `${variável,,}` é uma funcionalidade introduzida no Bash 4. É uma maneira direta de modificar strings sem chamar programas externos.

- `awk` é uma linguagem de programação e uma ferramenta poderosa para processamento de textos. Usar `awk` pode ser útil quando já se está trabalhando com ele em um pipeline de processamento de texto e se deseja manter a consistência.

Ao escolher um método, considere o contexto do seu script. Por exemplo, para scripts simples, `tr` é geralmente suficiente. Se estiver trabalhando dentro de um script Bash maior que faz uso de variáveis, então `${variável,,}` é limpo e eficiente. Se precisar fazer mais processamento de dados além de apenas converter para minúsculas, `awk` pode ser a melhor ferramenta para integrar essa funcionalidade.

## See Also

- [Bash Reference Manual](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)
- [GNU `tr` Manual](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html#tr-invocation)
- [AWK Manual](https://www.gnu.org/software/gawk/manual/gawk.html)

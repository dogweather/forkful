---
title:                "Convertendo uma string para letras minúsculas"
html_title:           "Fish Shell: Convertendo uma string para letras minúsculas"
simple_title:         "Convertendo uma string para letras minúsculas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que
Você já se encontrou em uma situação em que precisava converter uma string para letras minúsculas em seu código Fish Shell? Talvez você esteja comparando strings ou construindo uma função de pesquisa. Independentemente do motivo, a conversão para letras minúsculas é uma tarefa comum e útil na programação. Neste artigo, vamos explorar como realizar essa conversão usando o Fish Shell.

## Como Fazer
Para converter uma string para letras minúsculas em Fish Shell, podemos usar a função `string tolower`. Vamos dar uma olhada em um exemplo simples:

```
Fish Shell
# Definindo uma string
set minha_string "Olá Mundo!"

# Convertendo para letras minúsculas usando a função tolower
string tolower $minha_string

# Output: olá mundo!
```

Fácil, não é mesmo? A função `tolower` converte automaticamente toda a string para letras minúsculas.

Podemos até mesmo usar essa função para comparar strings independentemente de sua capitalização. Vamos ver um exemplo:

```Fish Shell
# Definindo duas string com capitalizações diferentes
set string1 "Hello World!"
set string2 "hello world!"

# Comparando as duas strings após a conversão para letras minúsculas
if test (string tolower $string1) = (string tolower $string2)
    echo "As strings são iguais!"
else
    echo "As strings são diferentes!"
end

# Output: As strings são iguais!
```

Neste exemplo, mesmo com as strings sendo de capitalizações diferentes, a conversão para letras minúsculas nos permitiu compará-las de forma precisa.

## Deep Dive
Por trás dos bastidores, a função `tolower` utiliza o conjunto de caracteres Unicode para converter as letras maiúsculas em suas respectivas letras minúsculas. Isso significa que ela funciona não apenas com o alfabeto inglês, mas também com caracteres especiais e acentos de outras línguas. Isso torna essa função muito útil em cenários multilíngues.

Além disso, a função `tolower` também é insensível a acentos. Isso significa que, por exemplo, a letra "É" será convertida para "é" durante a conversão para letras minúsculas. Portanto, podemos usar essa função com confiança, mesmo com strings que apresentam acentos.

## Veja Também
Para saber mais sobre as funções disponíveis no Fish Shell, confira a documentação oficial: https://fishshell.com/docs/current/index.html

Você também pode conferir alguns exemplos práticos de como usar o Fish Shell em nossos outros artigos aqui na plataforma. Bons códigos!
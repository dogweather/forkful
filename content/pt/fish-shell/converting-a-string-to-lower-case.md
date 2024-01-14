---
title:                "Fish Shell: Convertendo uma string para minúsculas."
simple_title:         "Convertendo uma string para minúsculas."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que:

Ao trabalhar com programação, podemos nos deparar com a necessidade de converter uma string para letras minúsculas. Isso pode ser útil para padronizar a formatação de textos em um projeto ou até mesmo para realizar comparações entre strings.

## Como fazer:

Converter uma string para letras minúsculas no fish shell é bastante simples. Podemos utilizar o comando "string tolower" seguido da string que deve ser convertida. Veja o exemplo abaixo:

```Fish Shell
string tolower "Exemplo de String Convertida"
```
Saída: exemplo de string convertida

Podemos também armazenar a string em uma variável e utilizar o comando "string tolower" dentro de um loop para converter várias strings de uma só vez. Veja o exemplo:

```Fish Shell
set strings "String 1" "String 2" "String 3"

for string in $strings
    set lowercase_string (string tolower $string)
    echo $lowercase_string
end
```
Saída: string 1
       string 2
       string 3

## Mergulho Profundo:

Ao converter uma string para letras minúsculas, é importante ter em mente que a conversão é realizada de acordo com a tabela ASCII, portanto, apenas caracteres com código acima de 64 serão convertidos. Por isso, é possível que alguns caracteres especiais não sejam convertidos.

Outro aspecto importante é que a conversão para minúsculas irá afetar apenas letras maiúsculas, sem alterar números, espaços ou outros caracteres especiais presentes na string.

## Veja também:

- [Documentação do comando "string tolower"](https://fishshell.com/docs/current/commands.html?#string-tolower)
- [Tutorial sobre strings no fish shell](https://www.linode.com/docs/guides/how-to-use-strings-in-fish-shell/)
- [Discussão sobre a conversão de strings em letras minúsculas no fish shell](https://github.com/fish-shell/fish-shell/issues/6071)
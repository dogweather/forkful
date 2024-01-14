---
title:    "Fish Shell: Usando expressões regulares"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar Expressões Regulares no Fish Shell?

Expressões regulares são uma ferramenta poderosa para buscar e manipular texto em um editor de texto ou no terminal. No Fish Shell, elas podem ser especialmente úteis para filtrar e processar dados de maneira eficiente e automatizada. Se você quer aumentar a sua produtividade e se tornar um programador mais eficiente, é importante saber como utilizar expressões regulares no seu dia a dia.

## Como usar Expressões Regulares no Fish Shell

As expressões regulares são formadas por padrões de texto que são usados para buscar e manipular informações específicas em um texto. No Fish Shell, podemos utilizar o comando `string match` para aplicar expressões regulares a uma variável de texto. Por exemplo:

```Fish Shell 
set texto "Hello World!"

if string match -r "W.*d" $texto

echo "Encontramos um padrão que começa com 'W' e termina com 'd'!"

end
```

Neste exemplo, o texto "Hello World!" é atribuído à variável `texto` e a expressão regular "W.*d" é utilizada para encontrar um padrão que começa com a letra "W" e termina com a letra "d". Como o texto corresponde a esse padrão, o comando `echo` é executado e a mensagem "Encontramos um padrão que começa com 'W' e termina com 'd'!" é exibida no terminal.

## Aprofundando-se nas Expressões Regulares

A sintaxe utilizada para criar expressões regulares no Fish Shell é semelhante à de outras linguagens de programação, mas existem algumas diferenças importantes. Por exemplo, no Fish Shell, não é necessário utilizar delimitadores como `/` ou `#` para indicar o início e o final da expressão regular.

Além disso, existem diversos metacaracteres que podem ser utilizados para buscar padrões específicos em um texto, como `.` para qualquer caractere, `*` para indicar que um padrão pode se repetir zero ou mais vezes, e `[ ]` para delimitar um conjunto de possíveis caracteres.

Para aprender mais sobre expressões regulares no Fish Shell, confira a documentação oficial e pratique criando diferentes padrões e utilizando-os em seu código.

## Veja também

- [Documentação oficial do Fish Shell sobre Expressões Regulares](https://fishshell.com/docs/current/cmds/string-match.html)
- [Tutorial de Expressões Regulares no Shell para iniciantes](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-fish-shell-configuration-files) 
- [Lista de recursos para se aprofundar em Expressões Regulares](https://www.regular-expressions.info/fish.html)
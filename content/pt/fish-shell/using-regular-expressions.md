---
title:    "Fish Shell: Utilizando expressões regulares"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por que utilizar expressões regulares em Fish Shell?

Expressões regulares são uma ferramenta poderosa para manipulação de textos em qualquer linguagem de programação, incluindo Fish Shell. Com elas, é possível buscar, substituir e extrair padrões específicos de uma string, tornando o processo de manipulação de dados mais eficiente e preciso. Além disso, as expressões regulares são uma habilidade valiosa para programadores de todos os níveis, já que são amplamente utilizadas em diversas áreas da computação.

## Como utilizar expressões regulares em Fish Shell

Para utilizar expressões regulares em Fish Shell, primeiro é preciso entender sua sintaxe básica. As expressões regulares são formadas por uma sequência de caracteres que definem um padrão a ser procurado em uma string. Esses caracteres podem ser letras, números, símbolos ou caracteres especiais, que possuem um significado específico na expressão regular.

Veja um exemplo de como buscar e substituir palavras em uma string utilizando expressões regulares em Fish Shell:

```Fish Shell
# Buscando e substituindo todas as ocorrências da palavra "gato" por "cachorro" em uma string
set texto "Eu tenho um gato preto e um gato branco"
echo $texto | sed -e 's/gato/cachorro/g'

# Saída: Eu tenho um cachorro preto e um cachorro branco
```

Outro exemplo é a extração de números em uma string utilizando expressões regulares:

```Fish Shell
# Extraindo números de uma string
set texto "A idade do João é 25 anos"
echo $texto | grep -o '\d\+'

# Saída: 25
```

## Aprofundando em expressões regulares

As expressões regulares possuem diversas formas de serem utilizadas e sua sintaxe pode parecer complexa no início. É importante conhecer bem os caracteres especiais e suas funções, além de praticar bastante para se familiarizar com a linguagem. Alguns comandos úteis em Fish Shell para utilizar expressões regulares são: sed, grep, awk e Perl.

Outro ponto importante é entender como os diferentes quantificadores e grupos de captura podem ser utilizados para buscar padrões mais específicos em uma string. É possível até mesmo criar expressões regulares para validar formatos de e-mail, números de telefone, entre outros.

Para aprofundar mais no assunto, recomendo a leitura dos seguintes artigos (em português):

- [Guia básico de expressões regulares em Fish Shell](https://www.linuxnatives.com.br/guia-de-expressoes-regulares-regex-em-fish-shell/)
- [Introdução às expressões regulares em Fish Shell](https://sempreupdate.com.br/introducao-as-expressoes-regulares-em-fish-shell/)
- [Expressões regulares em Fish Shell: dicas e exemplos práticos](https://www.grimmjow.me/expressoes-regulares-em-fish-shell-dicas-e-exemplos-praticos/)

## Veja também

- [Documentação do Fish Shell sobre expressões regulares](https://fishshell.com/docs/current/index.html#regular-expressions)
- [Tutorial interativo de expressões regulares em Fish Shell](https://regexr.com/fish)
- [Explicando expressões regulares com gatinhos fofos](https://github.com/zeeshanu/learn-regex)
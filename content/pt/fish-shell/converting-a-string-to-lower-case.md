---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

---

# Convertendo uma String para Caixa Baixa (lowercase) com Fish Shell: O Guia Prático

---

## O Que e Por Quê?

Converter uma string para caixa baixa envolve transformar todos os caracteres maiúsculos dela em minúsculos. Isso é feito para unificar a formatação dos datas, facilitando buscas, classificações e comparações.

## Como Fazer:

Para converter uma string para minúscula no Fish Shell, use o comando `string lower` no shell. Veja abaixo um exemplo simples:

```Fish Shell
$ string lower -a 'Meu Teste' 
```

A saída do comando acima será:

```Fish Shell
meu teste
```
Neste exemplo, '-a' é uma opção que torna a operação aplicável a cada argumento, e 'Meu Teste' é a string que estamos convertendo para minúsculo.

## Aprofundando

Converter caracteres para caixa baixa é uma prática antiga em tecnologia da informação, datando de quando os primeiros sistemas de bancos de dados foram criados. Isso simplifica a identificação precisa de dados.

Alternativas ao Fish Shell para a conversão de strings incluem outras linguagens de script de shell, como bash, zsh, ou até mesmo Python ou Perl.

No Fish Shell, o `string lower` é implementado através de uma função do C++ que é mapeada para cada caracter da string. Como tal, é eficiente e efetiva para operações em larga escala.

## Veja Também

- [Manual Oficial do Fish Shell](https://fishshell.com/docs/current/commands.html#string)
- [Recursos do Fish Shell](https://opensource.com/article/19/8/getting-started-fish-shell)
- [StackOverflow - Convertendo string para caixa baixa](https://stackoverflow.com/questions/tagged/fish+lowercase)

---

Espero que este guia prático seja útil para você, e feliz codificação!
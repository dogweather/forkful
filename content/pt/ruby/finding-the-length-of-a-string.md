---
title:    "Ruby: Encontrando o comprimento de uma string"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma tarefa comum em programação e é essencial para manipular dados e realizar operações em strings. Este é um conceito fundamental que todo programador Ruby deve conhecer.

## Como fazer

Para encontrar o comprimento de uma string em Ruby, usamos o método `length` ou `size`. Veja o exemplo abaixo:

```Ruby
s = "Olá, mundo!"
puts s.length # Output: 12
```

No exemplo acima, usamos o método `length` para encontrar o tamanho da string `s` e o resultado é 12. O método `size` funciona da mesma forma que `length` e pode ser usado de forma intercambiável.

Você também pode encontrar o comprimento de uma string usando um bloco. Veja o exemplo abaixo:

```Ruby
s = "Olá, mundo!"
puts s.each_char.count # Output: 12
```

Neste exemplo, usamos o método `each_char` para iterar por cada caractere da string `s` e usamos o método `count` para encontrar o número total de caracteres na string.

## Dive fundo

Ao encontrar o comprimento de uma string em Ruby, é importante lembrar que os espaços em branco são contados como caracteres. Isso pode ser uma surpresa para alguns programadores, mas é um comportamento padrão para a maioria das linguagens de programação. Além disso, o método `length` e o método `size` são métodos de instância e só podem ser usados em objetos de string.

Outra coisa a ter em mente é que o comprimento de uma string é sempre um número inteiro positivo. Mesmo se a string estiver vazia, o método `length` ou `size` retornará 0.

## Veja também

- [Documentação oficial do Ruby sobre strings] (https://ruby-doc.org/core-2.7.1/String.html)
- [Tutorial de Ruby para iniciantes] (https://www.ruby-lang.org/pt/documentation/quickstart/)

Esperamos que este artigo tenha ajudado você a entender como encontrar o comprimento de uma string em Ruby. Continue praticando e explorando os diferentes recursos da linguagem para se tornar um programador Ruby proficient. Obrigado por ler!
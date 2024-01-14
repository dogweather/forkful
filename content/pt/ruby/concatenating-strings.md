---
title:    "Ruby: Concatenando strings"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que

Você provavelmente já se deparou com a necessidade de juntar duas ou mais frases ou palavras para formar uma única linha de texto. Essa é a principal função da concatenação de strings em Ruby. Com ela, podemos criar mensagens personalizadas, formatar dados de forma legível e muito mais.

## Como fazer

Para concatenar strings em Ruby, utilizamos o operador de adição (+). Veja um exemplo de código abaixo:

```Ruby
nome = "Maria"
sobrenome = "Silva"
mensagem = "Olá, " + nome + " " + sobrenome + "! Seja bem-vinda."
puts mensagem
```

Este código irá imprimir a mensagem "Olá, Maria Silva! Seja bem-vinda." no console. Perceba que as variáveis nome e sobrenome foram concatenadas com a string "Olá, " e com o "!" para formar a mensagem completa.

Podemos também usar o método `concat` para concatenar strings. Veja o exemplo abaixo:

```Ruby
mensagem = "Hoje"
mensagem.concat(" é", " segunda-feira.")
puts mensagem
```

O resultado impresso será "Hoje é segunda-feira.", pois o método `concat` adicionou as strings "é" e "segunda-feira." à variável mensagem.

## Mergulho Profundo

É importante ressaltar que, ao concatenar strings em Ruby, estamos criando uma nova string e não alterando as strings originais. Isso significa que, se quisermos utilizar as strings originais novamente, devemos armazená-las em novas variáveis.

Além disso, podemos utilizar o método `<<` (shovel) para concatenar strings ao invés do operador de adição. Veja o exemplo abaixo:

```Ruby
mensagem = "Olá"
mensagem << " mundo!"
puts mensagem
```

O resultado será a string "Olá mundo!".

Outra técnica muito útil para concatenar strings é o uso de "interpolation" ou interpolação de strings. Nós podemos adicionar valores de variáveis diretamente em uma string, utilizando o sinal `#{}`. Veja o exemplo abaixo:

```Ruby
nome = "Pedro"
puts "Bem-vindo, #{nome}!"
```

O resultado será "Bem-vindo, Pedro!".

## Veja também

- [Documentação oficial do Ruby sobre strings](https://ruby-doc.org/core-2.7.2/String.html)
- [Tutorial de Ruby para iniciantes](https://www.ruby-lang.org/pt/documentation/quickstart/)
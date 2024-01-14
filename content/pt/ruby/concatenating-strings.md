---
title:                "Ruby: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

# Porque concatenação de strings é importante em Ruby

Na programação, muitas vezes nos deparamos com a necessidade de juntar duas ou mais strings em uma única string. Isso é conhecido como concatenação de strings e é uma tarefa muito comum em Ruby. Neste post, vamos explorar o porquê de usar concatenação de strings, como fazê-lo e algumas informações mais profundas sobre o assunto.

## Por que

A concatenação de strings é importante porque nos permite criar strings mais complexas, combinando informações de diferentes variáveis. Por exemplo, imagine que você está criando um programa de cadastro de usuários e precisa exibir uma mensagem de boas-vindas personalizada para cada usuário. Usando concatenação de strings, você pode combinar o nome e sobrenome do usuário para exibir a mensagem correta. Sem isso, seria necessário criar uma mensagem para cada usuário individualmente, o que seria muito trabalhoso e ineficiente.

## Como fazer

Em Ruby, existem algumas maneiras de concatenar strings. A forma mais comum é usando o operador de adição (+). Vamos ver um exemplo:

```Ruby
nome = "Ana"
sobrenome = "Silva"

mensagem = "Olá, " + nome + " " + sobrenome + ". Seja bem-vinda!"
puts mensagem # Saída: Olá, Ana Silva. Seja bem-vinda!
```

Outra forma de concatenar strings é usando o método `concat`. Este método pode ser chamado diretamente em uma string e aceita um ou mais argumentos que serão adicionados ao final da string original. Veja o exemplo:

```Ruby
mensagem = "Olá, "
mensagem.concat("Maria", " Pereira") # Adicionando duas palavras de uma vez

puts mensagem # Saída: Olá, Maria Pereira
```

## Deep Dive

Quando usamos o operador de adição (+) para concatenar strings, o Ruby cria um novo objeto string para armazenar o resultado. Isso significa que, se estivermos concatenando muitas strings em um programa, isso pode resultar em um uso desnecessário de memória. Para evitar isso, podemos usar o método `<<`, que adiciona os argumentos diretamente ao objeto original sem criar um novo. Também podemos usar esse método para fazer concatenação em loop, otimizando ainda mais o uso de memória. Veja o exemplo:

```Ruby
mensagem = "Entraram "
nomes = ["Ana", "Pedro", "Bruna"]

nomes.each do |nome|
  mensagem << nome << ", "
end

mensagem << "na sala."

puts mensagem # Saída: Entraram Ana, Pedro, Bruna, na sala.
```

## Veja também

- [Documentação oficial do Ruby sobre strings](https://ruby-doc.org/core-3.0.0/String.html)
- [Tutorial sobre strings em Ruby](https://www.rubyguides.com/2019/02/ruby-string-methods/)
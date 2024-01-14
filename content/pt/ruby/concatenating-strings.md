---
title:                "Ruby: Unindo cadeias de caracteres"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Concatenar strings é uma técnica útil para combinar múltiplas strings em uma só. Isso pode ser usado para criar mensagens personalizadas, construir URLs dinâmicos, criar strings de consulta e muito mais. 

## Como Fazer

Para concatenar strings em Ruby, podemos utilizar o operador de adição `+` ou o método `concat`. Vejamos alguns exemplos:

```
nome = "João"
sobrenome = "Silva"

puts nome + " " + sobrenome
# output: João Silva

endereco = "Rua " 
endereco.concat("Jardim Europa")

puts endereco
# output: Rua Jardim Europa
```

Podemos também utilizar a interpolação de strings, indicada pelo símbolo `$`, para criar strings dinâmicas:

```
idade = 30

puts "João tem #{idade} anos"
# output: João tem 30 anos
```

## Diving Profundo

Ao concatenar strings, é importante ter em mente que a união dos objetos resultará em uma nova string, sem alterar os objetos originais. Além disso, devemos ter cuidado com a adição acidental de espaços extras que podem alterar a saída desejada.

Outro detalhe importante é que o operador `+` e o método `concat` têm comportamentos diferentes quando se trata de objetos `nil`. Enquanto o operador irá levantar um erro, o método `concat` irá simplesmente não adicionar nada à string resultante. Para evitar esse tipo de problema, uma boa prática é verificar se os objetos estão vazios antes de concatená-los.

## Veja Também
- Guia completo sobre strings em Ruby: [https://www.rubyguides.com/2015/03/ruby-string/](https://www.rubyguides.com/2015/03/ruby-string/)
- Documentação oficial sobre a classe String: [https://ruby-doc.org/core-2.7.1/String.html](https://ruby-doc.org/core-2.7.1/String.html)
- Mais exemplos práticos de concatenação de strings: [https://www.tutorialspoint.com/ruby/ruby_strings.htm](https://www.tutorialspoint.com/ruby/ruby_strings.htm)
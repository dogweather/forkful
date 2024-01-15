---
title:                "Obtendo a data atual"
html_title:           "Ruby: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que

Se você está construindo um aplicativo ou script, é importante que ele tenha acesso à data atual. Isso permite que você forneça informações precisas e atualizadas para seus usuários.

## Como Fazer

Para obter a data atual em Ruby, você pode simplesmente usar o método `Time.now`. Isso retornará um objeto de data e hora que representa o momento exato em que o código é executado.

```Ruby
# Exemplo de código para obter a data atual
data_atual = Time.now

# Output: 2021-06-25 13:45:32 +0700
puts "A data atual é #{data_atual}"
```

Outra opção é usar o método `Date.today`, que retorna um objeto de data sem a hora e o fuso horário.

```Ruby
# Exemplo de código para obter a data atual sem hora e fuso horário
data_atual = Date.today

# Output: 2021-06-25
puts "A data atual é #{data_atual}"
```

Você também pode formatar a data de acordo com o seu gosto usando o método `strftime`, que aceita uma variedade de diretivas para representar diferentes partes da data e hora.

```Ruby
# Exemplo de código para formatar a data atual
data_atual = Time.now

# Output: 25/06/2021
puts "A data atual é #{data_atual.strftime('%d/%m/%Y')}"
```

## Profundidade

Por baixo dos panos, o método `Time.now` usa a classe `Time` da biblioteca padrão do Ruby, que por sua vez chama uma função C do sistema operacional para obter a data e a hora atual. Essa é apenas uma das muitas maneiras de obter a data atual em Ruby, mas é a mais simples e amplamente utilizada.

## Veja Também

- [Documentação oficial do método Time.now em Ruby](https://ruby-doc.org/core-2.7.2/Time.html#method-c-now)
- [Lista de diretivas para o método strftime em Ruby](http://strftime.net/)
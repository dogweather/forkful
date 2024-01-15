---
title:                "Extraindo Substrings"
html_title:           "Ruby: Extraindo Substrings"
simple_title:         "Extraindo Substrings"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Extrair substrings é uma técnica comum em programação que permite que você obtenha partes específicas de uma string de texto. Isso pode ser útil quando você precisa manipular ou processar informações em um formato específico.

## Como fazer

Extrair substrings em Ruby é simples e pode ser feito usando o método `slice` ou a notação de intervalo `[]`.

```
# Exemplo 1: ussando o método slice
string = "Olá, mundo!"
substring = string.slice(5, 5) # A partir do índice 5, obtenha 5 caracteres
puts substring # Output: mundo

# Exemplo 2: usando a notação de intervalo
string = "Olá, mundo!"
substring = string[0..3] # Do índice 0 ao 3 (incluindo), obtenha os caracteres
puts substring # Output: Olá

```

É importante notar que a contagem dos índices em Ruby começa a partir do 0, então o primeiro caractere de uma string tem o índice 0, o segundo caractere tem o índice 1 e assim por diante.

## Mergulho Profundo

Além do método `slice` e da notação de intervalo, também é possível extrair substrings usando expressões regulares. As expressões regulares são sequências de caracteres que formam um padrão e podem ser usadas para encontrar e extrair partes específicas de uma string.

```
string = "My email is example@email.com"
substring = string.match(/(\w+)@(\w+)\.com/)[1] # Procure por um padrão de email e obtenha o primeiro grupo de captura
puts substring # Output: email
```

As expressões regulares podem ser complexas, mas são extremamente úteis para extrair informações de uma string de maneira precisa e flexível.

## Veja também

- [Ruby String Class Documentation](https://ruby-doc.org/core-2.7.0/String.html)
- [Ruby Regular Expressions Tutorial](https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm)
---
title:                "Convertendo uma data em uma string"
html_title:           "Ruby: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

O que é e por que os programadores convertem uma data em uma string?

Converter uma data em uma string é um processo comum em programação, onde uma data é transformada em uma representação legível em formato de texto. Isso é feito para facilitar a comunicação com o usuário final e para possibilitar o armazenamento e manipulação de datas em diferentes formatos. Programadores geralmente realizam essa conversão em seus códigos para fornecer uma melhor experiência do usuário e garantir a precisão dos dados.

Como fazer:

Converter uma data em uma string em Ruby é bastante simples e pode ser feito de várias maneiras. Uma opção é usar o método `strftime`, que permite que você controle o formato da string de saída. Veja um exemplo:

```Ruby
date = Date.today
string = date.strftime("%d/%m/%Y")
puts string
```
```
21/12/2021
```
Uma outra opção é usar a classe `Date` e seu método `to_s`, que retorna uma string com formato de data padrão. Veja um exemplo:

```Ruby
date = Date.today
string = date.to_s
puts string
```
```
2021-12-21
```

A profundidade:

Converter uma data em uma string pode ser um processo bastante complexo, dependendo das necessidades específicas do programa. Antigamente, as datas eram armazenadas como números inteiros, que representavam o número de dias desde uma data de referência comum, geralmente 1 de janeiro de 1970. Com o avanço da tecnologia e diferentes sistemas operacionais, surgiram diferentes formatos de datas e métodos de armazenamento.

Além dos métodos mencionados acima, também é possível converter uma data em uma string através da utilização de bibliotecas de terceiros, como a `Chronic`, que permite a interpretação de datas em linguagem natural, como "amanhã" ou "próxima quinta-feira". Outra alternativa é utilizar o formato de data ISO 8601, que é um padrão internacional para representação de datas.

Veja também:

Para mais informações sobre como trabalhar com datas em Ruby, consulte a documentação oficial sobre a classe `Date` e seus métodos. Você também pode se aprofundar no assunto lendo sobre formatação de datas em Ruby e os diferentes formatos de armazenamento e conversão disponíveis.
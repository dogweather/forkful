---
title:                "Ruby: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Ao trabalhar com datas em programação Ruby, pode ser necessário converter uma data em uma string para fins de formatação ou exibição. Neste artigo, vamos explorar como fazer isso de uma maneira simples e eficiente.

## Como Fazer
Para converter uma data em uma string, podemos usar o método `strftime` da classe `Time`. Vamos dar uma olhada em alguns exemplos de formatação de datas:

```
# Formato padrão
Time.now.strftime("%d/%m/%Y")
=> 16/04/2021

# Mostrando o dia da semana
Time.now.strftime("%A, %d de %B")
=> Sexta-feira, 16 de abril

# Exibindo a hora
Time.now.strftime("%H:%M:%S")
=> 13:35:42
```

## Deep Dive
Ao usar o método `strftime`, devemos primeiro fornecer uma diretiva de formatação que corresponda ao tipo de informação que queremos extrair da data. Algumas das diretivas mais usadas são:

- `%Y` para o ano com quatro dígitos
- `%m` para o mês com dois dígitos
- `%d` para o dia com dois dígitos
- `%H` para a hora em formato de 24 horas
- `%M` para os minutos
- `%S` para os segundos
- `%A` para o nome do dia da semana
- `%B` para o nome do mês

Existem muitas outras diretivas disponíveis, e é possível combiná-las para criar uma string de data personalizada. É importante notar que a maioria das diretivas será impressa com zeros à esquerda caso o valor seja menor que 10, mas isso pode ser alterado adicionando um hífen antes da diretiva.

## Veja Também
- [Documentação Oficial do Ruby sobre o método `strftime`](https://ruby-doc.org/core-3.0.0/Time.html#method-i-strftime)
- [Tutorial sobre formatação de datas em Ruby](https://www.rubyguides.com/2015/05/formatting-dates-in-ruby/)
- [Outras formas de converter datas em Ruby](https://www.codecademy.com/learn/learn-ruby/modules/introduction-to-ruby-u/cheatsheet)
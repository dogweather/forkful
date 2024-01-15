---
title:                "Converter uma data em uma cadeia de caracteres"
html_title:           "Ruby: Converter uma data em uma cadeia de caracteres"
simple_title:         "Converter uma data em uma cadeia de caracteres"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes, em programação, é necessário converter datas em formato de texto para facilitar o armazenamento ou a apresentação para o usuário. Neste artigo, vamos explorar como fazer isso usando Ruby, uma linguagem de programação versátil e popular.

## Como Fazer

Converter uma data em formato de texto pode ser feito de diversas maneiras em Ruby. Vamos ver algumas opções usando diferentes métodos da biblioteca padrão da linguagem.

### Usando o método `strftime`

Uma forma simples e eficiente de converter uma data em uma string é usando o método `strftime` da classe `Time`. Esse método permite especificar o formato desejado para a saída. Veja o exemplo abaixo:

```Ruby
data = Time.now
puts data.strftime("%d/%m/%Y")
```

Neste caso, estamos usando o formato `%d/%m/%Y`, que corresponde ao dia, mês e ano separados por barras, resultando em uma saída como `30/03/2021`. Você pode ver a lista completa dos formatos disponíveis na documentação oficial da Ruby.

### Usando o método `to_s`

Outra forma de converter uma data em string é usando o método `to_s`, que já está presente na maioria das classes de data e hora em Ruby. Veja o exemplo abaixo:

```Ruby
data = Time.now
puts data.to_s
```

Neste caso, a saída será em um formato padrão, que pode variar dependendo do sistema operacional e da configuração de idioma. Por exemplo, pode ser `Tue Mar 30 22:11:19 2021` em um sistema com idioma em inglês ou `ter mar 30 22:11:19 2021` em um sistema com idioma em português.

### Usando o método `iso8601`

O método `iso8601` é útil quando precisamos de uma data em formato de texto para ser compatível com outros sistemas ou APIs, pois ele segue o padrão ISO 8601 para datas e horas. Veja um exemplo abaixo:

```Ruby
data = Time.now
puts data.iso8601
```

A saída será algo como `2021-03-30T22:15:51-03:00`, seguindo o formato `YYYY-MM-DDTHH:MM:SS-ZZZZ`.

## Deep Dive

Por baixo dos panos, a conversão de uma data em string é feita usando formatação de texto e conversões entre tipos de dados. O Ruby é uma linguagem com foco em produtividade e simplicidade, o que torna esse processo bastante intuitivo e descomplicado.

Além dos métodos mencionados acima, existem também outras opções que podem ser úteis em situações específicas, como o uso de formatação com sufixos (como `swrite` ou `rwfuzzytime`) ou o uso da gem `strftime`. Esses recursos podem ser explorados com mais profundidade em outros artigos ou tutoriais.

## Veja Também

- [Documentação oficial da Ruby](https://ruby-doc.org/core-3.0.0/Time.html#method-i-strftime)
- [Lista de formatos para o método `strftime`](https://apidock.com/ruby/strftime)
- [ISO 8601 - Wikipédia](https://pt.wikipedia.org/wiki/ISO_8601)
- [Gem `strftime`](https://rubygems.org/gems/strftime)
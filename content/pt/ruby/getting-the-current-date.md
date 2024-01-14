---
title:    "Ruby: Obtendo a data atual."
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual?

Obter a data atual é uma tarefa bastante utilizada na programação. Isso é especialmente importante para programas que precisam se atualizar constantemente com base na data ou para registrar informações em um banco de dados com a data atual. Além disso, obter a data atual também é útil para fins de visualização de dados em aplicativos ou sites.

## Como Fazer:

Para obter a data atual em Ruby, podemos usar o método `Time.now`. Este método retorna um objeto `Time`, que contém informações sobre a data e hora atual. Aqui está um exemplo:

```ruby
# Retorna o objeto Time atual
data = Time.now

# Imprime o objeto Time no formato padrão
puts data

# Imprime a data atual em um formato personalizado
puts data.strftime("%d/%m/%Y")
```

A saída deste código seria:

```
2021-04-05 15:30:12 +0300
05/04/2021
```

Podemos ver que o objeto `Time` contém informações como data, hora, fuso horário e formato de data e hora. Ao usar o método `strftime`, podemos especificar um formato personalizado para a data e hora, como mostrado no exemplo acima. Para obter mais informações sobre os formatos possíveis, você pode consultar a [documentação oficial](https://ruby-doc.org/core-3.0.0/Time.html#method-i-strftime) do método `strftime`.

## Profundidade:

O método `Time.now` é bastante simples, mas sua implementação pode ser um pouco complicada. Ele utiliza o sistema operacional subjacente para obter a data e hora atual. Outro fator importante a ser mencionado é que a data e hora dependem da configuração do fuso horário no sistema operacional. Isso significa que, se a configuração do fuso horário estiver incorreta, a data e hora que você obterá também será incorreta.

Além disso, o método `Time.now` também é afetado pelas mudanças de horário de verão. Isso significa que, se você obtiver a data atual durante a mudança de horário de verão, a hora obtida pode não ser a correta. Para evitar esses problemas, é recomendável utilizar um serviço de tempo externo, como o [Network Time Protocol (NTP)](https://pt.wikipedia.org/wiki/Network_Time_Protocol).

## Veja também:

- [Documentação do Ruby sobre a classe Time](https://ruby-doc.org/core-3.0.0/Time.html)
- [Documentação do método strftime](https://ruby-doc.org/core-3.0.0/Time.html#method-i-strftime)
- [Como obter o horário atual a partir de um servidor NTP em Ruby](https://stackoverflow.com/questions/31318483/get-current-time-from-ntp-server-in-ruby)
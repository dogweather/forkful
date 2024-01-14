---
title:    "Ruby: Transformando uma data em uma string"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que

Se você é um desenvolvedor Ruby, provavelmente já se deparou com a necessidade de converter uma data em uma string em algum momento do seu trabalho. Isso pode ser útil para exibir informações de datas em formato legível para usuários ou para formatação de dados em relatórios. Neste artigo, vamos discutir como realizar essa conversão de forma eficiente em Ruby.

## Como fazer

Para converter uma data em uma string em Ruby, podemos usar o método `strftime`, que nos permite especificar o formato em que queremos exibir a data. Por exemplo, se quisermos exibir a data atual no formato "dia/mês/ano", podemos fazer da seguinte maneira:

```Ruby
data_atual = Time.now
data_string = data_atual.strftime("%d/%m/%Y")
puts data_string
```

Isso nos dará a saída "21/04/2020". Podemos escolher diferentes formatos para exibir a data, como "mês/dia/ano", "ano-mês-dia", entre outros. O símbolo `%d` representa o dia, `%m` representa o mês e `%Y` representa o ano. Além disso, podemos adicionar outros caracteres para formatar a saída como desejarmos. Por exemplo, se quisermos adicionar o nome completo do dia da semana, podemos usar `%A` e a saída será "terça-feira, 21 de abril de 2020".

```Ruby
data_string = data_atual.strftime("%A, %d de %B de %Y")
puts data_string
```

Isso nos dará a saída "terça-feira, 21 de abril de 2020". Existem muitas opções de formatação disponíveis e você pode encontrar mais informações na documentação oficial da biblioteca de data e hora do Ruby.

## Mergulho Profundo

Agora que já sabemos como utilizar o método `strftime` para converter uma data em uma string, é importante entender como ele funciona por trás dos panos. Em essência, ele utiliza uma lista de códigos de formato predefinidos e, ao receber uma data, substitui esses códigos pelos valores correspondentes. Por exemplo, `%d` será substituído pelo dia, `%m` pelo mês e assim por diante.

Também é importante lembrar que o método `strftime` só funciona com objetos do tipo Time ou DateTime no Ruby. Outros tipos de dados, como strings ou integers, precisam ser convertidos para um desses tipos antes de usar o `strftime`.

## Veja também

- [Documentação oficial da biblioteca de data e hora do Ruby](https://ruby-doc.org/stdlib-2.7.1/libdoc/time/rdoc/Time.html#method-i-strftime)
- [Guia de referência rápida para formatação de datas no Ruby](https://alvinalexander.com/technology/ruby-date-time-format-tutorial-strftime-strptime)
- [Outros métodos úteis para manipulação de datas em Ruby](https://medium.com/better-programming/incredible-date-manipulation-in-ruby-for-absolute-beginners-f661c643240d)
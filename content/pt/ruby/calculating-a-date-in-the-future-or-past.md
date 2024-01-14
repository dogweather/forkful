---
title:    "Ruby: Calculando uma data no futuro ou passado"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que

Calcular datas no futuro ou no passado é uma tarefa comum para programadores de Ruby. Seja para agendar uma tarefa, rastrear a validade de um item ou simplesmente fazer uma previsão, saber como calcular datas é uma habilidade essencial para qualquer programador.

## Como Fazer

Existem várias maneiras de calcular datas no futuro ou no passado em Ruby. Aqui estão alguns exemplos usando a classe `DateTime`:

```
# Calcular uma data 7 dias no futuro
DateTime.now + 7

# Calcular uma data 2 semanas no passado
DateTime.now - 14

# Calcular uma data 1 mês no futuro
DateTime.now >> 1
```

Esses são apenas alguns exemplos básicos. Existem várias opções para personalizar suas datas, como adicionar ou subtrair dias, semanas, meses ou anos. Você também pode usar métodos como `strftime` para formatar sua data de acordo com suas necessidades.

## Deep Dive

Calcular datas no futuro ou no passado envolve entender como a classe `DateTime` funciona em Ruby. Essa classe possui vários métodos úteis para manipulação de datas, como `strptime` que permite converter uma string em uma data e `parse` que converte uma string em um objeto de data e hora.

Você também pode usar a classe `Time` para calcular datas, que é semelhante à `DateTime` mas possui alguns métodos diferentes. Ambas as classes possuem muitas opções para personalizar suas datas e são amplamente utilizadas em projetos Ruby.

## Veja Também

Para mais informações sobre como calcular datas em Ruby, confira a documentação oficial da classe `DateTime` e `Time`.

- [Documentação Oficial da Classe DateTime](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/DateTime.html)
- [Documentação Oficial da Classe Time](https://ruby-doc.org/stdlib-3.0.0/libdoc/time/rdoc/Time.html)
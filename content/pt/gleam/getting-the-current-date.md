---
title:    "Gleam: Obtendo a data atual"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Por que utilizar Gleam para obter a data atual?

Você já se perguntou como os aplicativos e sites conseguem exibir a data atual? Ou talvez você esteja trabalhando em um projeto que precisa dessa funcionalidade? Independentemente do motivo, a linguagem de programação Gleam oferece uma maneira simples e eficiente de obter a data atual em seus programas.

# Como fazer isso em Gleam

Para obter a data atual em Gleam, utilizamos a função `Date.utc_now()` que retorna um objeto `Date` contendo a data e hora atuais.

```
Gleam: obtenhaData
import Date
  Date.utc_now()
```

Agora, podemos usar esse objeto para acessar informações específicas, como o dia, mês, ano, entre outros.

```
Gleam: obtenhaDataDetalhada
import Date
  let dataAtual = Date.utc_now()
  let dia = Date.day(dataAtual)
  let mes = Date.month(dataAtual)
  let ano = Date.year(dataAtual)
```

O código acima irá retornar o dia, mês e ano atuais em variáveis separadas, que podem ser usadas para formar uma data personalizada de acordo com suas necessidades.

# Aprofundando no assunto

O objeto `Date` também possui outras funções úteis para trabalhar com datas, como `Date.diff()` que calcula a diferença entre duas datas, `Date.to_iso8601()` que converte a data em uma string no formato ISO 8601 e `Date.from_utc_iso8601()` que converte uma string no formato ISO 8601 em um objeto `Date`.

Além disso, é possível utilizar a biblioteca `Timex` para ter acesso a mais funcionalidades relacionadas a data e hora em Gleam.

# Veja também

- Documentação oficial da Gleam sobre a função `Date.utc_now()`: https://gleam.run/modules/date#utc_now
- Exemplo de uso da função `Date.utc_now()` em um programa Gleam: https://github.com/lpil/gleam/blob/master/examples/date.gleam
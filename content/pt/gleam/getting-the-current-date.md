---
title:                "Obtendo a data atual."
html_title:           "Gleam: Obtendo a data atual."
simple_title:         "Obtendo a data atual."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que & Por que?

Obter a data atual é uma tarefa comum na programação, pois permite que os programadores criem programas dinâmicos e interativos que exibem informações atualizadas para os usuários.

## Como fazer:

```Gleam
import Time

let current_date = Time.now()

// Saída: 2021-10-21T10:23:50.621026Z
```

O código acima utiliza o módulo 'Time' para chamar a função 'now' e obter a data e hora atuais. É importante lembrar que a data e hora serão apresentadas em UTC (Tempo Universal Coordenado).

```Gleam
import Time

let current_date = Time.now()
let formatted_date = Time.format(current_date, "%Y-%m-%d")

// Saída: 2021-10-21
```

Se desejar exibir apenas a data atual sem a hora, é possível utilizar a função 'format' para formatar a data de acordo com um padrão específico. No exemplo acima, estamos utilizando "%Y-%m-%d" para exibir apenas ano, mês e dia separados por hífen.

## Mergulho profundo:

Embora possível, não é recomendado calcular a data e hora atual por conta própria, pois isso pode levar a imprecisões e erros. Portanto, utilizar um módulo como 'Time' facilita o processo e garante a precisão dos dados.

No entanto, existem outras abordagens para obter a data atual, como utilizar APIs externas ou integrar a aplicação com serviços de terceiros. Cabe ao programador decidir qual é a melhor opção para o seu projeto.

## Veja também:

- Documentação do módulo 'Time': https://gleam.run/modules/time.html
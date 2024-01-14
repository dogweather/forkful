---
title:                "Elixir: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Por que usar Elixir para obter a data atual?

Se você é um programador ou está interessado em aprender uma nova linguagem de programação, provavelmente já ouviu falar de Elixir. Esta linguagem de programação funcional, baseada em Erlang, vem ganhando popularidade nos últimos anos devido à sua escalabilidade, eficiência e tolerância a falhas.

Mas talvez você esteja se perguntando: por que eu escolheria Elixir para obter a data atual? Bem, existem várias razões pelas quais Elixir é uma ótima escolha para essa tarefa. Primeiro, sua sintaxe é elegante e fácil de entender, o que a torna ideal para iniciantes e experientes. Além disso, Elixir é altamente otimizado para tarefas concorrentes, o que significa que ele é perfeito para lidar com várias solicitações ao mesmo tempo. Mas agora, vamos mergulhar e ver como podemos obter a data atual em Elixir.

## Como obter a data atual em Elixir

Para obter a data atual em Elixir, usaremos a função `Date.utc_today/0`. Esta função retorna uma data no formato AAAA-MM-DD e é baseada no tempo universal coordenado (UTC). Para usá-la, primeiro precisamos importar o módulo `Date` em nosso código:

```Elixir
import Date
```

Agora podemos chamar a função `Date.utc_today/0` para obter a data atual:

```Elixir
data_atual = Date.utc_today()
```

Podemos então imprimir essa data no terminal usando a função `IO.inspect/1`:

```Elixir
IO.inspect(data_atual) # saída: ~D[2021-08-11]
```

## Profundidade no funcionamento da função `Date.utc_today/0`

Agora que sabemos como obter a data atual em Elixir, vamos dar uma olhada em como essa função funciona por baixo dos panos. A função `Date.utc_today/0` usa uma chamada de sistema operacional para obter a data atual do sistema. Em seguida, ela converte a data de um formato interno para o formato desejado (AAAA-MM-DD) e retorna esse valor.

Além disso, é importante notar que a função `Date.utc_today/0` sempre retorna a data atual no fuso horário UTC, independentemente do fuso horário em que seu sistema se encontra. Isso garante que a data seja consistente, independentemente de onde seu código esteja sendo executado.

# Veja também

Agora que você sabe como obter a data atual em Elixir, aqui estão alguns links úteis para você continuar aprendendo sobre esta linguagem de programação incrível:

- [Documentação oficial do Elixir](https://elixir-lang.org/getting-started/introduction.html)
- [Guia de estilo de código Elixir](https://github.com/gusaiani/elixir_style_guide/blob/master/README_pt_BR.md)
- [Lista de recursos e ferramentas recomendados para Elixir](https://github.com/h4cc/awesome-elixir)

Esperamos que este artigo tenha ajudado você a entender um pouco mais sobre Elixir e como obter a data atual usando esta linguagem funcional. Então vá em frente, experimente e divirta-se codando em Elixir!
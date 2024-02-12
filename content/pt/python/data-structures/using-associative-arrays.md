---
title:                "Usando arrays associativos"
aliases: - /pt/python/using-associative-arrays.md
date:                  2024-01-30T19:12:38.357981-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando arrays associativos"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O quê & Por quê?

Arrays associativos, conhecidos em Python como dicionários, mapeiam chaves para valores, facilitando a recuperação, modificação ou rastreamento de dados por um identificador único. Programadores os utilizam pela eficiência no acesso a elementos e sua flexibilidade na representação de estruturas de dados complexas.

## Como fazer:

Criar um dicionário em Python é simples. Você envolve pares de chave-valor em chaves `{}`, com chaves e valores separados por dois pontos:

```Python
# Criar um array associativo (dicionário)
my_dict = {"name": "John", "age": 30, "city": "New York"}
print(my_dict)
```

Saída:
```
{'name': 'John', 'age': 30, 'city': 'New York'}
```

Acessar um valor pela sua chave é simples:

```Python
# Acessar um valor
print(my_dict["name"])
```

Saída:
```
John
```

Adicionar ou atualizar elementos é feito atribuindo um valor a uma chave:

```Python
# Adicionar um novo par chave-valor
my_dict["email"] = "john@example.com"
# Atualizar um valor
my_dict["age"] = 31
print(my_dict)
```

Saída:
```
{'name': 'John', 'age': 31, 'city': 'New York', 'email': 'john@example.com'}
```

Para iterar sobre os itens do dicionário:

```Python
# Iterar através dos pares chave-valor
for key, value in my_dict.items():
    print(f"{key}: {value}")
```

Saída:
```
name: John
age: 31
city: New York
email: john@example.com
```

## Aprofundando

Arrays associativos em Python, ou dicionários, foram introduzidos para fornecer uma estrutura de dados para acesso e manipulação de dados de forma eficiente. Ao contrário de sequências, que são indexadas por um intervalo de números, dicionários são indexados por chaves, que podem ser de qualquer tipo imutável. Essa escolha de design torna os dicionários idealmente adequados para tabelas de busca rápida onde chaves mapeiam para valores únicos.

Historicamente, dicionários em Python têm sido implementados usando uma tabela hash, garantindo que o tempo médio de complexidade para operações de busca, inserção e exclusão seja O(1). Em Python 3.6 e posteriores, dicionários também mantêm a ordem de inserção dos itens, combinando os benefícios das tabelas hash com a previsibilidade da ordem de inserção vista em estruturas de dados ordenadas.

Embora dicionários sejam incrivelmente versáteis, em alguns casos especializados, alternativas como `collections.defaultdict` ou `collections.OrderedDict` (antes do Python 3.7) podem ser preferíveis. `defaultdict` é particularmente útil quando você precisa de um dicionário para retornar um valor padrão para chaves inexistentes, simplificando certos tipos de lógica condicional. No entanto, com a contínua melhoria e evolução do Python, a classe de dicionário integrada frequentemente permanece a escolha preferida para arrays associativos devido à sua robustez e a conveniência que oferece imediatamente.

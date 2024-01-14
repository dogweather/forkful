---
title:                "Python: Trabalhando com json."
simple_title:         "Trabalhando com json."
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON?
JSON (JavaScript Object Notation) é uma forma simples e leve de armazenar e trocar dados entre sistemas. Ele é amplamente utilizado na programação, especialmente em aplicações web, devido à sua fácil leitura e escrita. Trabalhar com JSON permite que os programadores organizem e manipulem dados de maneira eficiente, facilitando o desenvolvimento de aplicações.

## Como utilizar JSON em Python
JSON é suportado nativamente pelo Python, tornando seu uso muito simples. Para começar, importe o módulo `json` no seu código:

```Python
import json
```

Em seguida, você pode carregar dados de um arquivo JSON em uma estrutura de dados Python usando a função `load()` e especificando o caminho do arquivo como parâmetro:

```Python
with open("dados.json") as f:
    dados = json.load(f)
```

Você também pode converter uma estrutura de dados Python em um arquivo JSON usando a função `dump()`:

```Python
dados = {"nome": "João", "idade": 25}
with open("dados.json", "w") as f:
    json.dump(dados, f)
```

O JSON também pode ser utilizado para enviar e receber dados em formato texto através de requisições HTTP. Para isso, você pode utilizar a biblioteca `requests` e a função `json()` para converter a resposta em um objeto JSON:

```Python
import requests

resposta = requests.get("https://api.exemplo.com/dados")
dados = resposta.json()
```

## Aprofundando em JSON
Além disso, o JSON possui algumas características interessantes que podem ser exploradas ao trabalhar com ele em Python. Por exemplo, você pode definir opções adicionais ao utilizar as funções `load()` e `dump()`, como `indent` para identação do arquivo ou `sort_keys` para ordenar as chaves.

Além disso, é possível iterar sobre um objeto JSON convertido em um dicionário Python para acessar suas chaves e valores:

```Python
for chave, valor in dados.items():
    print(chave, valor)
```

Outra vantagem de trabalhar com JSON em Python é a capacidade de combinar diferentes métodos, como utilizar a biblioteca `pandas` para carregar um arquivo JSON como um dataframe e analisar seus dados.

## Veja também
- [Documentação oficial do módulo JSON em Python](https://docs.python.org/3/library/json.html)
- [Tutorial completo de JSON em Python](https://realpython.com/python-json/)
- [Exemplos de uso de JSON em Python](https://www.w3schools.com/python/python_json.asp)
---
title:                "Trabalhando com json"
html_title:           "Fish Shell: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

# O que é e por que usar JSON?

JSON (JavaScript Object Notation) é um formato de intercâmbio de dados leve e fácil de ler e escrever. Ele é frequentemente usado em programação para armazenar e transmitir dados estruturados entre diferentes sistemas. Os programadores usam JSON porque é uma maneira eficiente e flexível de organizar e gerenciar informações.

# Como usar JSON no Fish Shell

Fish Shell possui um módulo interno para trabalhar com JSON. Para acessá-lo, você pode usar o comando `json` seguido do subcomando desejado.

```
Fish Shell$ json get '{"name": "Maria", "age": 25}' name
Maria
```

Neste exemplo, usamos o subcomando `get` para recuperar o valor da chave "name" do objeto JSON fornecido. O resultado será "Maria".

# Aprofundando no JSON

JSON foi criado por Douglas Crockford em 2001 e se tornou uma opção popular para representar dados estruturados devido à sua simplicidade e uso de sintaxe familiar baseada em JavaScript. Alguns dos principais concorrentes de JSON são XML e YAML.

Para trabalhar com JSON em Fish Shell, você também pode precisar usar módulos externos, como o `jq`, que fornece uma série de recursos adicionais para manipular e formatar dados JSON.

# Veja também

- [Documentação oficial do Fish Shell: JSON](https://fishshell.com/docs/current/cmds/json.html)
- [Documentação oficial do jq](https://stedolan.github.io/jq/)
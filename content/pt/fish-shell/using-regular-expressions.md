---
title:                "Usando expressões regulares"
html_title:           "Fish Shell: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

O que é e Por Que Usar Expressões Regulares?

Expressões regulares são padrões de texto utilizados para procurar e manipular strings de caracteres em programas de computador. Programadores usam expressões regulares para facilitar a busca e substituição de padrões específicos de texto, tornando o processo mais rápido e eficiente.

Como Fazer:

```Fish Shell
# Para procurar um padrão específico de texto, use o comando "grep" seguido de um padrão e um arquivo
grep "padrão" arquivo.txt

# Para substituir um padrão por outro, utilize o comando "sed" com a sintaxe "s/padrão/substituição/"
sed "s/padrão/substitução/" arquivo.txt

# Para evitar erros de digitação, use a opção "-i" para fazer as mudanças diretamente no arquivo
sed -i "s/padrão/substituição/" arquivo.txt
```

Deep Dive:

Expressões regulares tiveram suas origens em ferramentas de busca e edição de texto na década de 1950. Hoje em dia, existem várias alternativas para manipulação de texto, como AWK, Perl e Python, mas expressões regulares ainda são amplamente utilizadas devido à sua simplicidade e eficiência. A linguagem de programação Fish Shell possui suporte nativo para expressões regulares, facilitando o seu uso em tarefas rotineiras de programação.

Veja também:

- [Documentação oficial da Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial sobre Expressões Regulares na Fish Shell](https://fishshell.com/docs/current/tutorial.html#using-regular-expressions)
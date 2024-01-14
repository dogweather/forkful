---
title:                "Fish Shell: Convertendo uma string em minúsculas."
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que
A conversão de strings para letras minúsculas é uma tarefa muito comum em programação, e é importante para garantir a consistência e a formatação adequada dos dados. Além disso, pode ser necessário converter strings em letras minúsculas para fins de comparação, classificação ou pesquisa.

## Como fazer
Para converter uma string em letras minúsculas no Fish Shell, podemos usar o comando `string tolower`, seguido da string que queremos converter. Por exemplo:

```
Fish Shell

string tolower "Olá Mundo!"

# saída: olá mundo!
```

No exemplo acima, a string "Olá Mundo!" foi convertida para letras minúsculas e a saída foi "olá mundo!".

## Profundidade
Existem algumas coisas importantes a serem consideradas ao converter uma string para letras minúsculas. Primeiro, é importante lembrar que as letras acentuadas também serão convertidas em suas versões minúsculas, por exemplo "Ê" será convertido para "ê".

Além disso, em alguns casos, pode ser necessário especificar o idioma para garantir que a conversão para minúsculas seja feita corretamente. Isso pode ser feito adicionando o parâmetro `-l` e o código do idioma após o comando `string tolower`. Por exemplo:

```
Fish Shell

string tolower -l pt "É Preciso Ser Homem"

# saída: é preciso ser homem
```

No exemplo acima, especificamos o idioma como português (código "pt") e a saída foi convertida para "é preciso ser homem".

## Veja também
- [Documentação oficial do Fish Shell para o comando string tolower](https://fishshell.com/docs/current/commands.html#string-tolower)
- [Exemplo de script em Fish Shell usando o comando string tolower](https://github.com/fish-shell/fish-shell/blob/master/share/doc/fish/functions/tolower.fish)
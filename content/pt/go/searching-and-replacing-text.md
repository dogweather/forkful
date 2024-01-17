---
title:                "Buscando e substituindo texto"
html_title:           "Go: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que é e por que os programadores fazem
Substituir e buscar texto é uma tarefa comum para os programadores, em que eles procuram e substituem um determinado texto por outro em um documento ou em um código. Isso é frequentemente usado quando os desenvolvedores precisam alterar uma parte específica de um código ou documento sem ter que fazer alterações manuais extensas. 

## Como fazer
Usando o Go, substituir e buscar texto é bastante simples. Primeiro, importe o pacote "strings", que contém as funções necessárias para manipular texto. Em seguida, você pode usar a função "ReplaceAll" para substituir todo o texto no documento ou use a função "Replace" para substituir apenas uma instância específica. Aqui está um exemplo de código que substitui todas as letras "a" por "b" em uma string:

```
Go import "strings"
 
s := "banana"
new := strings.ReplaceAll(s, "a", "b")
fmt.Println(new)

// Saída: bbnbnb
```

Note que o texto original não é alterado, e sim uma nova string é criada com as substituições.

## Mais informações
Substituir e buscar texto tem sido uma tarefa importante para os programadores desde os primeiros dias da programação. Antes dos computadores e das linguagens de programação modernas, os programadores precisavam fazer alterações manuíais em grandes pilhas de papéis contendo código. Felizmente, hoje temos tecnologias e ferramentas que tornam essa tarefa mais eficiente.

Uma alternativa à função "ReplaceAll" é a função "ReplaceAllString" que pode ser usada para procurar e substituir texto usando expressões regulares. Além disso, o pacote "strings" também possui outras funções úteis para manipular texto, como "Split" e "Join".

## Veja também
Para saber mais sobre a sintaxe e os recursos do pacote "strings", confira a documentação oficial do Go [aqui](https://golang.org/pkg/strings/). Além disso, existem muitos recursos disponíveis online para a aprendizagem de expressões regulares e outras técnicas de manipulação de texto. Faça uma pesquisa e continue aprimorando suas habilidades de programação em Go!
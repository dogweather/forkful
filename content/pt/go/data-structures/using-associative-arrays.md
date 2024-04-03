---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:49.646819-07:00
description: "Arrays associativos, conhecidos como maps em Go, permitem que voc\xEA\
  \ armazene pares chave-valor onde cada chave \xFAnica mapeia para um valor. Programadores\u2026"
lastmod: '2024-03-13T22:44:46.052862-06:00'
model: gpt-4-0125-preview
summary: "Arrays associativos, conhecidos como maps em Go, permitem que voc\xEA armazene\
  \ pares chave-valor onde cada chave \xFAnica mapeia para um valor."
title: Utilizando arrays associativos
weight: 15
---

## O Que & Porquê?

Arrays associativos, conhecidos como maps em Go, permitem que você armazene pares chave-valor onde cada chave única mapeia para um valor. Programadores usam maps para recuperação de dados de forma eficiente, modificação, e para manter uma coleção de elementos que podem ser acessados rapidamente usando chaves únicas.

## Como fazer:

Criar e inicializar um map em Go pode ser feito de várias maneiras. Aqui está um exemplo básico para começar:

```go
package main

import "fmt"

func main() {
    // Declarando e inicializando um map
    colors := map[string]string{
        "vermelho":   "#FF0000",
        "verde": "#00FF00",
        "azul":  "#0000FF",
    }

    fmt.Println(colors)
    // Saída: map[azul:#0000FF verde:#00FF00 vermelho:#FF0000]
}
```

Para adicionar ou atualizar elementos, você atribui um valor a uma chave assim:

```go
colors["branco"] = "#FFFFFF"
fmt.Println(colors)
// Saída: map[azul:#0000FF verde:#00FF00 vermelho:#FF0000 branco:#FFFFFF]
```

Acessar um valor pela sua chave é simples:

```go
fmt.Println("O código hex para vermelho é:", colors["vermelho"])
// Saída: O código hex para vermelho é: #FF0000
```

Para deletar um elemento, use a função `delete`:

```go
delete(colors, "vermelho")
fmt.Println(colors)
// Saída: map[azul:#0000FF verde:#00FF00 branco:#FFFFFF]
```

Iterar sobre um map é feito usando um loop for:

```go
for cor, hex := range colors {
    fmt.Printf("Chave: %s Valor: %s\n", cor, hex)
}
```

Lembre-se, maps em Go são desordenados. A ordem de iteração não é garantida.

## Aprofundamento

Em Go, maps são implementados como tabelas hash. Cada entrada no map consiste de dois itens: uma chave e um valor. A chave é hasheada para armazenar a entrada, o que permite operações de tempo constante para um pequeno conjunto de dados e complexidade de tempo médio de O(1) com hashing adequado, que pode degradar para O(n) no pior caso com muitas colisões de hash.

Uma nota significativa para novos programadores de Go é que os tipos de map são tipos de referência. Isso significa que quando você passa um map para uma função, quaisquer mudanças feitas no map dentro dessa função são visíveis para o chamador. Isso é diferente de, digamos, passar uma struct para uma função, onde a struct é copiada a menos que seja passada por um ponteiro.

Embora maps sejam incrivelmente versáteis e eficientes para a maioria dos casos de uso envolvendo arrays associativos, em aplicações críticas de desempenho, pode ser benéfico usar estruturas de dados com características de desempenho mais previsíveis, especialmente se as distribuições de chave podem causar colisões frequentes.

Outra alternativa a considerar é o `sync.Map`, disponível desde Go 1.9, projetado para casos de uso onde chaves são escritas apenas uma vez mas lidas muitas vezes, oferecendo melhorias de eficiência nesses cenários. No entanto, para aplicações convencionais de Go, o uso regular de map é idiomático e frequentemente a abordagem recomendada pela sua simplicidade e suporte direto na linguagem.

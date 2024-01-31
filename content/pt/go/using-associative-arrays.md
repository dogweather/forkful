---
title:                "Usando arrays associativos"
date:                  2024-01-30T19:11:22.282754-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando arrays associativos"

category:             "Go"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Porquê?

Arrays associativos, conhecidos como mapas em Go, permitem armazenar e acessar dados com pares de chave-valor. Eles são essenciais para gerenciar coleções onde você pode buscar valores rapidamente por uma chave única, simplificando a manipulação e recuperação de dados em seus programas.

## Como fazer:

Em Go, os mapas são fáceis de usar. Aqui está um guia simples para começar:

1. **Declarando e Inicializando Mapas**

```Go
package main

import "fmt"

func main() {
    // Inicializa um mapa vazio com chaves do tipo string e valores do tipo int
    var scores map[string]int
    fmt.Println(scores) // Imprime: map[]

    // Declarando e inicializando um mapa não vazio
    colors := map[string]string{
        "red": "#ff0000",
        "green": "#00ff00",
    }
    fmt.Println(colors) // Imprime: map[green:#00ff00 red:#ff0000]
}
```

2. **Adicionando e Acessando Elementos**

```Go
func main() {
    fruits := make(map[string]int)
    fruits["apples"] = 5
    fruits["bananas"] = 10

    fmt.Println(fruits["apples"]) // Imprime: 5
}
```

3. **Iterando Sobre Mapas**

```Go
func main() {
    pets := map[string]string{"dog": "bark", "cat": "meow"}

    for key, value := range pets {
        fmt.Printf("%s goes %s\n", key, value)
    }
    // A ordem do output pode variar, pois os mapas não garantem ordem.
}
```

4. **Deletando Elementos**

```Go
func main() {
    meals := map[string]int{"breakfast": 300, "lunch": 600}
    fmt.Println(meals) // Antes da deleção

    delete(meals, "lunch")
    fmt.Println(meals) // Após a deleção
}
```

## Aprofundando

Introduzidos no Go 1, os mapas fornecem uma maneira integrada de lidar com arrays associativos de forma eficiente. Ao contrário de slices, que são coleções ordenadas, mapas são desordenados. Isso significa que a ordem de iteração sobre os elementos do mapa não é garantida ser a mesma em execuções diferentes, um compromisso pela sua capacidade de lidar com pares de chave-valor dinamicamente e com significativa flexibilidade.

Por baixo do capô, o Go implementa mapas como tabelas hash, garantindo que a complexidade média das operações de acesso, inserção e deleção seja O(1), na maioria das circunstâncias. No entanto, vale ressaltar que essa eficiência pode variar com base em fatores como colisões de hash.

Para casos de uso que requerem a travessia de chaves ordenadas, pode-se considerar combinar mapas com slices ou explorar pacotes de terceiros que ofereçam estruturas de dados adicionais como mapas ordenados ou árvores. Apesar de suas limitações, os mapas de Go são uma ferramenta poderosa e essencial para muitos cenários de programação.

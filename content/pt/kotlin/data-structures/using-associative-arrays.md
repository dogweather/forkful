---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:02.003853-07:00
description: "Arrays associativos, ou mapas, em Kotlin s\xE3o cole\xE7\xF5es que armazenam\
  \ pares de chave-valor. Os programadores os usam para organizar e recuperar dados\
  \ de\u2026"
lastmod: 2024-02-19 22:05:05.574532
model: gpt-4-0125-preview
summary: "Arrays associativos, ou mapas, em Kotlin s\xE3o cole\xE7\xF5es que armazenam\
  \ pares de chave-valor. Os programadores os usam para organizar e recuperar dados\
  \ de\u2026"
title: Usando arrays associativos
---

{{< edit_this_page >}}

## O quê & Por quê?

Arrays associativos, ou mapas, em Kotlin são coleções que armazenam pares de chave-valor. Os programadores os usam para organizar e recuperar dados de forma eficiente com base em chaves únicas, facilitando o gerenciamento de informações.

## Como fazer:

Criar e usar um mapa em Kotlin é simples. Aqui está um guia rápido sobre como fazê-lo:

```Kotlin
fun main() {
    // Criando um mapa mutável
    val frutas = mutableMapOf("a" to "Maçã", "b" to "Banana")

    // Adicionando elementos
    frutas["o"] = "Laranja" // Usando operação de indexação
    frutas.put("g", "Uva") // Usando o método put

    // Acessando elementos
    println(frutas["a"])  // Saída: Maçã
    println(frutas["b"])  // Saída: Banana

    // Removendo elementos
    frutas.remove("b")
    
    // Iterando sobre o mapa
    for ((chave, valor) in frutas) {
        println("$chave -> $valor")
    }
    // Saída de amostra:
    // a -> Maçã
    // o -> Laranja
    // g -> Uva
}
```

## Mergulho Profundo

Os mapas de Kotlin vêm diretamente de sua interoperabilidade com Java, onde os mapas são uma parte essencial das coleções. No entanto, Kotlin aprimora a usabilidade deles fornecendo interfaces mutáveis (`MutableMap`) e somente leitura (`Map`), ao contrário da interface `Map` unificada do Java. Essa distinção deixa claro se uma coleção é destinada à modificação ou não.

Um detalhe importante sobre a implementação do mapa de Kotlin é a distinção explícita entre mapas mutáveis e imutáveis, o que enfatiza o foco da linguagem na imutabilidade e segurança de thread.

Embora os mapas sejam altamente úteis, Kotlin também oferece outras coleções, como listas e conjuntos, cada uma com seu próprio caso de uso. Por exemplo, listas mantêm a ordem e permitem duplicatas, tornando-as ideais para acessar elementos por índice, enquanto conjuntos garantem unicidade, mas não mantêm a ordem. A escolha entre usar um mapa, lista ou conjunto depende dos requisitos específicos da sua aplicação, como a necessidade de acesso baseado em chave ou preservação de ordem.

Quanto a alternativas melhores, se o desempenho for crucial, especialmente com grandes coleções, considere usar estruturas de dados especializadas e mais eficientes fornecidas por bibliotecas externas que são otimizadas para casos de uso particulares, como acesso simultâneo ou ordenação.

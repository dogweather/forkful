---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:02.003853-07:00
description: "Como fazer: Criar e usar um mapa em Kotlin \xE9 simples. Aqui est\xE1\
  \ um guia r\xE1pido sobre como faz\xEA-lo."
lastmod: '2024-03-13T22:44:46.534467-06:00'
model: gpt-4-0125-preview
summary: "Criar e usar um mapa em Kotlin \xE9 simples."
title: Usando arrays associativos
weight: 15
---

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

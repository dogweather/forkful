---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:27.899929-07:00
description: "Extrair substrings envolve recuperar por\xE7\xF5es espec\xEDficas de\
  \ uma string com base em suas posi\xE7\xF5es. Programadores frequentemente realizam\
  \ essa opera\xE7\xE3o para\u2026"
lastmod: '2024-03-13T22:44:46.048751-06:00'
model: gpt-4-0125-preview
summary: "Extrair substrings envolve recuperar por\xE7\xF5es espec\xEDficas de uma\
  \ string com base em suas posi\xE7\xF5es. Programadores frequentemente realizam\
  \ essa opera\xE7\xE3o para\u2026"
title: Extraindo substrings
weight: 6
---

## O que & Por quê?

Extrair substrings envolve recuperar porções específicas de uma string com base em suas posições. Programadores frequentemente realizam essa operação para processar ou manipular dados de texto de forma eficiente, como analisar entradas, validar formatos ou preparar saídas.

## Como fazer:

Em Go, o tipo `string` é um slice somente leitura de bytes. Para extrair substrings, utiliza-se primariamente a sintaxe de `slice`, juntamente com a função integrada `len()` para verificação de comprimento e o pacote `strings` para operações mais complexas. Aqui está como você pode fazer isso:

### Fatiamento Básico

```go
package main

import (
    "fmt"
)

func main() {
    str := "Hello, World!"
    // Extrai "World"
    subStr := str[7:12]
    
    fmt.Println(subStr) // Saída: World
}
```

### Usando o Pacote `strings`

Para extração avançada de substrings, como extrair strings após ou antes de uma substring específica, você pode usar o pacote `strings`.

```go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "name=John Doe"
    // Extrai substring após "="
    subStr := strings.SplitN(str, "=", 2)[1]
    
    fmt.Println(subStr) // Saída: John Doe
}
```

É essencial notar que strings em Go são codificadas em UTF-8 e um slice direto de bytes nem sempre resultará em strings válidas se incluírem caracteres de múltiplos bytes. Para suporte a Unicode, considere usar `range` ou o pacote `utf8`.

### Lidando com Caracteres Unicode

```go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    str := "Hello, 世界"
    // Encontrando substring considerando caracteres Unicode
    runeStr := []rune(str)
    subStr := string(runeStr[7:])
    
    fmt.Println(subStr) // Saída: 世界
}
```

## Aprofundamento

Extrair substrings em Go é direto, graças à sua sintaxe de slice e biblioteca padrão abrangente. Historicamente, linguagens de programação anteriores forneciam funções ou métodos mais diretos para lidar com tal manipulação de texto. No entanto, a abordagem do Go enfatiza segurança e eficiência, particularmente com suas strings imutáveis e manuseio explícito de caracteres Unicode através de runes.

Embora o fatiamento direto beneficie da eficiência de desempenho, herda a complexidade de lidar diretamente com caracteres UTF-8. A introdução do tipo `rune` permite que programas em Go manuseiem texto Unicode de forma segura, tornando-o uma alternativa poderosa para aplicações internacionais.

Além disso, programadores vindos de outras linguagens podem sentir falta de funções integradas de manipulação de strings de alto nível. Ainda assim, os pacotes `strings` e `bytes` na biblioteca padrão do Go oferecem um conjunto rico de funções que, embora requeiram um pouco mais de código padrão, fornecem opções poderosas para o processamento de strings, incluindo a extração de substrings.

Em essência, as escolhas de design do Go em torno da manipulação de strings refletem seus objetivos de simplicidade, desempenho e segurança ao lidar com dados de texto modernos e internacionalizados. Embora possa requerer um pequeno ajuste, o Go oferece ferramentas eficazes e eficientes para lidar com a extração de substrings e muito mais.

---
aliases:
- /pt/go/capitalizing-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:52:35.201642-07:00
description: "Capitalizar uma string envolve transformar o primeiro caractere de uma\
  \ string dada em mai\xFAscula, se ele estiver em min\xFAscula, garantindo que a\
  \ string se\u2026"
lastmod: 2024-02-18 23:08:57.650205
model: gpt-4-0125-preview
summary: "Capitalizar uma string envolve transformar o primeiro caractere de uma string\
  \ dada em mai\xFAscula, se ele estiver em min\xFAscula, garantindo que a string\
  \ se\u2026"
title: Capitalizando uma string
---

{{< edit_this_page >}}

## O Que & Por Quê?

Capitalizar uma string envolve transformar o primeiro caractere de uma string dada em maiúscula, se ele estiver em minúscula, garantindo que a string se destaque ou adira a normas gramaticais específicas. Programadores frequentemente realizam esta operação para formatar entradas de usuários, exibir nomes próprios ou garantir a consistência dos dados em aplicações de software.

## Como fazer:

Em Go, o pacote `strings` não fornece uma função direta para capitalizar apenas a primeira letra de uma string. Portanto, combinamos a função `strings.ToUpper()`, que converte uma string para maiúscula, com fatiamento para alcançar nosso objetivo. Veja como fazer:

```go
package main

import (
    "fmt"
    "strings"
    "unicode/utf8"
)

func CapitalizeFirst(str string) string {
    if str == "" {
        return ""
    }
    // Verificar se o primeiro caractere já está em maiúscula.
    if utf8.ValidString(str) && unicode.IsUpper([]rune(str)[0]) {
        return str
    }
    
    // Converter o primeiro caractere para maiúscula
    r, size := utf8.DecodeRuneInString(str)
    return string(unicode.ToUpper(r)) + str[size:]
}

func main() {
    example := "hello, World!"
    fmt.Println(CapitalizeFirst(example)) // Saída: "Hello, World!"
}
```

Esta função verifica se a string está vazia ou se o primeiro caractere já está em maiúscula. Ela usa o pacote `unicode/utf8` para lidar corretamente com caracteres Unicode, garantindo que nossa função funcione com uma ampla gama de entradas, além do ASCII básico.

## Aprofundamento

A necessidade de capitalizar strings em Go sem uma função integrada pode parecer uma limitação, especialmente para programadores vindos de linguagens onde as funções de manipulação de strings são mais abrangentes. Esta restrição encoraja o entendimento do manejo de strings e a importância do Unicode no desenvolvimento de software moderno.

Historicamente, as linguagens de programação evoluíram em seu tratamento de strings, com as primeiras linguagens muitas vezes ignorando a internacionalização. A abordagem do Go, embora exija um pouco mais de código para tarefas aparentemente simples, garante que os desenvolvedores tenham em mente os usuários globais desde o início.

Existem bibliotecas fora da biblioteca padrão, como `golang.org/x/text`, oferecendo capacidades de manipulação de texto mais sofisticadas. No entanto, o uso dessas deve ser ponderado em relação à adição de dependências externas ao seu projeto. Para muitas aplicações, os pacotes `strings` e `unicode/utf8` da biblioteca padrão fornecem ferramentas suficientes para a manipulação de strings de forma eficaz e eficiente, como mostrado em nosso exemplo. Isso mantém os programas Go enxutos e bem gerenciáveis, ecoando a filosofia de simplicidade e clareza do Go.

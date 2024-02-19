---
aliases:
- /pt/go/removing-quotes-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:17.275975-07:00
description: "Remover aspas de uma string em Go \xE9 sobre eliminar as aspas de abertura\
  \ e fechamento (`\"` ou `'`) de uma determinada string. Programadores frequentemente\u2026"
lastmod: 2024-02-18 23:08:57.655684
model: gpt-4-0125-preview
summary: "Remover aspas de uma string em Go \xE9 sobre eliminar as aspas de abertura\
  \ e fechamento (`\"` ou `'`) de uma determinada string. Programadores frequentemente\u2026"
title: Removendo aspas de uma string
---

{{< edit_this_page >}}

## O Que & Por Quê?

Remover aspas de uma string em Go é sobre eliminar as aspas de abertura e fechamento (`"` ou `'`) de uma determinada string. Programadores frequentemente precisam realizar essa tarefa para higienizar a entrada do usuário, analisar dados de texto de maneira mais eficaz ou preparar strings para processamento adicional que exige conteúdo sem aspas.

## Como Fazer:

Go oferece várias abordagens para remover aspas de uma string, mas um dos métodos mais diretos é usar as funções `Trim` e `TrimFunc` fornecidas pelo pacote `strings`. Eis como fazer isso:

```go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	quotedString := `"Esta é uma string 'entre aspas'"`

	// Usando strings.Trim para remover aspas específicas
	unquoted := strings.Trim(quotedString, `"'`)
	fmt.Println("Usando strings.Trim:", unquoted)

	// Abordagem personalizada usando strings.TrimFunc para mais controle
	unquotedFunc := strings.TrimFunc(quotedString, func(r rune) bool {
		return r == '"' || r == '\''
	})
	fmt.Println("Usando strings.TrimFunc:", unquotedFunc)
}
```

Este exemplo demonstra duas abordagens para remover as aspas duplas (`"`) e simples (`'`). A função `strings.Trim` é mais simples e funciona bem quando você sabe exatamente quais caracteres remover. Por outro lado, `strings.TrimFunc` oferece mais flexibilidade, permitindo que você especifique uma função personalizada para decidir quais caracteres remover. A saída de amostra do código acima é:

```
Usando strings.Trim: Esta é uma string 'entre aspas'
Usando strings.TrimFunc: Esta é uma string 'entre aspas'
```

Ambos os métodos removem efetivamente as aspas de abertura e fechamento da string.

## Mergulho Profundo

As funções `Trim` e `TrimFunc` do pacote `strings` fazem parte da extensa biblioteca padrão do Go, projetada para oferecer capacidades poderosas, porém simplificadas, de manipulação de strings sem a necessidade de pacotes de terceiros. Historicamente, a necessidade de manipular e processar strings de forma eficiente decorre do foco principal do Go em servidores de rede e analisadores de dados, onde o processamento de strings é uma tarefa comum.

Um aspecto notável dessas funções é a sua implementação baseada em runes (representação do Go para um ponto de código Unicode). Esse design permite que elas lidem de forma transparente com strings contendo caracteres multibyte, tornando a abordagem do Go para manipulação de strings robusta e amigável ao Unicode.

Embora o uso direto de `Trim` e `TrimFunc` para remover aspas seja conveniente e idiomático em Go, vale mencionar que para tarefas de processamento de strings mais complexas (por exemplo, aspas aninhadas, aspas escapadas), expressões regulares (via o pacote `regexp`) ou análise manual podem oferecer soluções melhores. No entanto, essas alternativas vêm com considerações de complexidade e desempenho aumentadas. Por isso, para a simples remoção de aspas, os métodos demonstrados equilibram bem entre simplicidade, desempenho e funcionalidade.

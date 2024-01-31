---
title:                "Removendo aspas de uma string"
date:                  2024-01-26T03:39:28.828863-07:00
model:                 gpt-4-0125-preview
simple_title:         "Removendo aspas de uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Remover aspas de uma string significa se livrar daqueles incômodos caracteres de aspa dupla ou simples que envolvem seu texto real. Fazemos isso para higienizar dados, prevenir erros de análise ou preparar o texto para processamento adicional sem o excesso desnecessário de aspas.

## Como fazer:

Aqui está a maneira simples de mandar as aspas para o espaço em Go:

```go
package main

import (
	"fmt"
	"strings"
)

func removeQuotes(s string) string {
	return strings.Trim(s, "'\"")
}

func main() {
	quotedString := "\"Hello, World!\""
	fmt.Println("Original:", quotedString)

	unquotedString := removeQuotes(quotedString)
	fmt.Println("Unquoted:", unquotedString)
}
```

A saída ficará assim, sem as aspas:

```
Original: "Hello, World!"
Unquoted: Hello, World!
```

## Aprofundando

Lá atrás, quando os formatos de dados e intercâmbio não eram padronizados, aspas em strings podiam causar estragos. Ainda podem, especialmente em JSON ou ao inserir strings em bancos de dados. O pacote `strings` em Go vem equipado com uma função `Trim`, que elimina não apenas espaços em branco mas qualquer caractere que você não goste.

Por que não Regex? Bem, `Trim` é mais rápido para tarefas simples, mas se suas strings estão brincando de esconde-esconde com aspas em lugares estranhos, regex pode ser sua artilharia pesada:

```go
import "regexp"

func removeQuotesWithRegex(s string) string {
	re := regexp.MustCompile(`^["']|["']$`)
	return re.ReplaceAllString(s, "")
}
```

É como escolher entre tesouras e uma motosserra; escolha a ferramenta adequada para o trabalho.

## Veja Também

Para mais informações sobre o pacote `strings` e suas ferramentas poderosas:
- [Pacote strings](https://pkg.go.dev/strings)

Para empunhar o poder das expressões regulares em Go:
- [Pacote regexp](https://pkg.go.dev/regexp)

Quer mergulhar na filosofia do corte de strings?
- [O Método Trim](https://blog.golang.org/strings)

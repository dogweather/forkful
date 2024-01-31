---
title:                "Capitalizando uma string"
date:                  2024-01-19
html_title:           "Bash: Capitalizando uma string"
simple_title:         "Capitalizando uma string"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizar uma string é o processo de transformar todas as letras de uma palavra ou frase em maiúsculas. Programadores fazem isso para padronizar dados, para ênfase visual, ou para atender a requisitos específicos como códigos ou identificadores.

## How to:
```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    texto := "olá, mundo!"
    textoCapitalizado := strings.ToUpper(texto)
    fmt.Println(textoCapitalizado)
}
```

Saída esperada:
```
OLÁ, MUNDO!
```

## Deep Dive
Em Go, a função `ToUpper` é fornecida no pacote `strings` e é amplamente utilizada para capitalizar strings. Historicamente, a necessidade de capitalização data dos primeiros dias da computação, quando a consistência e a clareza dos dados eram fundamentais e as limitações de exibição às vezes só permitiam o uso de maiúsculas.

Alternativas incluem a capitalização manual de caracteres usando a tabela de códigos ASCII ou Unicode, mas isso é mais complicado e sujeito a erros. Em Go, `ToUpper` lida com essas complexidades para você, considerando a variação de idiomas e caracteres especiais.

Detalhadamente, `ToUpper` itera sobre cada caractere da string e aplica a transformação para maiúscula. Isso é feito utilizando a definição de letras maiúsculas da Unicode, que é muito mais abrangente que a tabela ASCII, suportando uma vasta gama de idiomas e símbolos.

## See Also
- Documentação oficial do pacote `strings`: https://pkg.go.dev/strings
- Um guia detalhado sobre Unicode e UTF-8 em Go: https://blog.golang.org/strings

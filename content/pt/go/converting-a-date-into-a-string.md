---
title:    "Go: Convertendo uma data em uma string."
keywords: ["Go"]
---

{{< edit_this_page >}}

Por que converter uma data em uma string em Go?

Ao trabalhar com datas em um programa, muitas vezes precisamos convertê-las em strings para exibi-las em um formato específico ou para armazená-las em um banco de dados. Felizmente, o idioma Go possui uma função integrada para facilitar essa conversão. Neste artigo, aprenderemos por que é necessário converter uma data em uma string e como fazer isso em Go.

## Como Fazer

Para converter uma data em uma string em Go, usamos a função ```format.Time``` e especificamos o layout desejado. Por exemplo, se quisermos exibir a data atual no formato "Dia, Mês Ano", podemos escrever o seguinte código:

```
import "fmt"
import "time"

func main() {
    data := time.Now()
    dataString := data.Format("Monday, January 2, 2006")
    fmt.Println(dataString)
}

```

Este código primeiro importa os pacotes "fmt" e "time" e, em seguida, obtém a data atual usando ```time.Now ()```. Em seguida, usamos a função ```Format ()``` para converter a data em uma string no formato especificado. No exemplo acima, usamos o layout "Monday, January 2, 2006", que é baseado no padrão de referência do Go para datas. Ao executar este código, obteremos a seguinte saída:

```Monday, September 6, 2021```

Também é possível especificar um formato de data personalizado usando uma combinação de caracteres específicos, como no exemplo abaixo:

```
import "fmt"
import "time"

func main() {
    data := time.Now()
    dataString := data.Format("02/01/2006")
    fmt.Println(dataString)
}
```

Este código exibirá a data atual no formato "Dia/Mês/Ano", resultando em algo do tipo: ```06/09/2021```.

## Deep Dive

A função ```time.Format ()``` oferece uma grande flexibilidade ao trabalhar com datas em Go. Além dos layouts pré-definidos, também podemos especificar nosso próprio formato usando caracteres específicos, como no exemplo anterior. Alguns dos caracteres comumente usados ​​para formatar datas são:

- D: dia do mês (2 dígitos)
- M: mês (2 dígitos)
- Y: ano (4 dígitos)
- H: hora (24 horas)
- I: hora (12 horas)
- P: AM / PM
- S: segundo
- d: dia do mês (1 dígito)
- m: mês (1 dígito)
- y: ano (2 dígitos)
- h: hora (12 horas sem zero à esquerda)
- i: hora (12 horas com zero à esquerda)
- p: AM / PM minúsculo
- s: segundo (com zeros à direita)

Além desses caracteres, também é possível usar outros formatos e também combinações deles. É importante observar que, ao usar a função ```Format ()```, os formatos devem ser especificados exatamente como estão, caso contrário a conversão não será bem-sucedida.

## Veja também

- [Documentação oficial do Go sobre a função time.Format ()](https://pkg.go.dev/time#Time.Format)
- [Tutorial sobre como trabalhar com datas em Go](https://www.calhoun.io/working-with-dates-and-times-in-go/)
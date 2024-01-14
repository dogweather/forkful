---
title:    "Go: Convertendo uma string para minúsculas."
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por que
Você já se deparou com uma situação em que precisava comparar duas strings, mas uma delas estava em caixa alta e a outra em caixa baixa? Ou talvez você precisasse formatar corretamente a entrada de texto de um usuário antes de utilizá-la em seu programa? Nestes casos, é necessário converter uma string para caixa baixa. Aprender a fazer isso em Go pode facilitar muito a sua vida!

## Como fazer
Para converter uma string para caixa baixa em Go, podemos utilizar a função integrada `strings.ToLower()`. Veja um exemplo de código abaixo:

```Go
texto := "Exemplo de TEXTO em CAIXA ALTA"
textoMinusculo := strings.ToLower(texto)
fmt.Println(textoMinusculo)
```
O resultado deste código será:
```
exemplo de texto em caixa alta
```
Como você pode ver, a função `strings.ToLower()` transforma todas as letras maiúsculas em minúsculas.

Além disso, também é possível converter strings para caixa baixa utilizando a função `strings.ToLowerSpecial()` que permite definir um critério específico para a conversão, como por exemplo, transformar letras com acentos em letras minúsculas sem acento. Veja um exemplo do uso desta função:

```Go
texto := "Ótimo Exemplo DE tExTo"
textoMinusculo := strings.ToLowerSpecial(unicode.TurkishCase, texto)
fmt.Println(textoMinusculo)
```
O resultado será:
```
ótimo exemplo de texto
```

## Mergulho Profundo
Caso você queira entender melhor como a função `strings.ToLower()` funciona, é possível dar uma olhada no código fonte da biblioteca padrão do Go. Lá você encontrará a implementação da função, que basicamente itera sobre cada caractere da string e utiliza a função `unicode.ToLower()` para transformá-lo em caixa baixa.

É importante lembrar que a função `strings.ToLower()` retorna uma nova string com a conversão realizada, mas não altera a string original. Portanto, é necessário atribuir o valor retornado para uma nova variável ou sobrescrever a variável original. Algo importante a se notar é que esta função pode não funcionar da maneira esperada para caracteres de outros alfabetos que não o latino.

## Veja também
- Documentação oficial do pacote `strings` em Go: https://golang.org/pkg/strings/
- Artigo sobre manipulação de strings em Go: https://github.com/golang/go/wiki/SliceTricks#strings
- Guia rápido sobre Unicode em Go: https://blog.golang.org/strings
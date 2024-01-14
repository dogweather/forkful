---
title:    "Kotlin: Removendo caracteres que correspondam a um padrão"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por que

Às vezes, em um programa Kotlin, pode ser necessário excluir certos caracteres que correspondem a um determinado padrão. Isso pode ser útil para limpar dados de entrada ou para fins de validação.

## Como Fazer

Para excluir caracteres correspondentes a um padrão em Kotlin, podemos usar o método `replace` da classe `String`. Este método aceita como argumentos o padrão a ser substituído e a string de substituição. Por exemplo:

```
Kotlin
val texto = "Olá, sou um texto!#$% Me exclua!"
val output = texto.replace(Regex("[^A-Za-z0-9 ]"), "")
println(output)
```
Output:
```
Olá sou um texto Me exclua
```

Neste exemplo, usamos a classe `Regex` para definir o padrão que queremos substituir. O padrão `[A-Za-z0-9 ]` significa que queremos manter apenas letras, números e espaços na string. O método `replace` então substitui todos os caracteres que não se encaixam no padrão por uma string vazia, deixando apenas os caracteres desejados na saída.

## Profundando

Além disso, podemos usar outros métodos da classe `Regex` para ser mais específicos na definição do padrão. Por exemplo, podemos usar o método `matches` para verificar se uma string inteira corresponde ao padrão ou o método `find` para encontrar a primeira ocorrência do padrão na string.

Além disso, o Kotlin também oferece suporte a expressões regulares para nos ajudar a criar padrões mais complexos para substituição. Podemos consultar a documentação para aprender mais sobre o uso de expressões regulares com Kotlin.

## Veja Também

- [Documentação do Kotlin sobre expressões regulares](https://kotlinlang.org/docs/regexp.html)
- [Tutorial de expressões regulares no Kotlin](https://www.baeldung.com/kotlin/regex)
- [Vídeo tutorial sobre expressões regulares no Kotlin](https://www.youtube.com/watch?v=ba9BpN7xin0)
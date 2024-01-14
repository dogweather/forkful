---
title:    "Swift: Convertendo uma string em maiúsculas"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

Capitalizar uma string é uma tarefa comum em muitos aplicativos e programas escritos em Swift. Isso pode ser usado para formatar títulos ou nomes próprios em letras maiúsculas. Aprender a codificar esse recurso pode ser útil para melhorar a aparência e a funcionalidade do seu código.

## Como fazer

Para capitalizar uma string em Swift, você precisa usar o método `capitalized` em uma instância de `String`. Aqui está um exemplo de código que mostra como fazer isso:

```Swift
var minhaString = "teste"
print(minhaString.capitalized)
```

A saída desse código seria "Teste", com a primeira letra em maiúscula. Você também pode usar o método `uppercased` se quiser apenas tornar todas as letras maiúsculas, ou `lowercased` para torná-las todas minúsculas.

## Mergulho profundo

O método `capitalized` usa as regras de capitalização do idioma padrão do sistema. Isso significa que, se você estiver executando seu aplicativo em um dispositivo configurado com o idioma espanhol, por exemplo, a saída seria "Prueba". No entanto, você também pode especificar um idioma específico usando o parâmetro `locale` do método. Isso é útil quando você precisa formatar uma string de acordo com uma regra de capitalização específica de um idioma diferente do idioma padrão do sistema.

## Veja também

- Documentação oficial da Apple para o método `capitalized`: [https://developer.apple.com/documentation/foundation/nsstring/1434241-capitalized](https://developer.apple.com/documentation/foundation/nsstring/1434241-capitalized)
- Artigo de blog da Hacking with Swift sobre capitalizar strings: [https://www.hackingwithswift.com/example-code/strings/how-to-capitalize-the-first-letter-of-a-string](https://www.hackingwithswift.com/example-code/strings/how-to-capitalize-the-first-letter-of-a-string)
- Vídeo tutorial do canal CodeWithChris sobre capitalizar strings em Swift: [https://www.youtube.com/watch?v=Iy7uNi6HTxM](https://www.youtube.com/watch?v=Iy7uNi6HTxM)
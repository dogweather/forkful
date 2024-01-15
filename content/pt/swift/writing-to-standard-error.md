---
title:                "Escrevendo para o erro padrão"
html_title:           "Swift: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Por que escrever no erro padrão?

Você pode estar se perguntando por que alguém se preocuparia em escrever para o erro padrão em Swift. A resposta é simples: ao escrever para o erro padrão, você tem uma maneira rápida e fácil de ver informações sobre o seu código durante a execução.

# Como fazer isso

Escrever para o erro padrão é bastante simples em Swift. Você pode usar o operador `print` e especificar o parâmetro `to` como `StandardError`. Dessa forma, todo o texto que você deseja imprimir será enviado para o erro padrão. Veja um exemplo abaixo:

```Swift
print("Este é um erro", to: &StandardError.stream)
```

O resultado disso seria uma mensagem de erro impressa no console, indicando que há algo errado com o seu código. Claro, você pode substituir "Este é um erro" por qualquer mensagem relevante para a sua situação específica.

# Mergulho profundo

Escrever para o erro padrão pode ser especialmente útil quando você está depurando e quer ver o valor de certas variáveis em um determinado ponto do seu código. Por exemplo, se você quiser saber o valor de uma variável no meio de uma função, pode usar `print` para imprimir essa informação no erro padrão e verificá-la facilmente.

Além disso, é possível personalizar ainda mais a maneira como as informações são exibidas no erro padrão, usando o tipo `TextOutputStream` e implementando o método `write`. Dessa forma, você pode formatar os dados de maneira mais precisa antes de serem exibidos.

# Veja também

- [Documentação oficial da Apple sobre escrever para o erro padrão em Swift](https://developer.apple.com/documentation/swift/standarderror)
- [Tutorial sobre como usar a saída de texto personalizada no erro padrão em Swift](https://medium.com/@rootcis/creating-a-custom-logger-in-swift-6dc5188669c9)
- [Vídeo demonstrando como usar a saída de erro padrão em Swift](https://youtu.be/2yBYXxeKxVI)
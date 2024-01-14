---
title:                "Gleam: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

# Por que utilizar a impressão de saída de depuração em Gleam?

A impressão de saída de depuração é uma técnica valiosa para programadores que desejam monitorar o fluxo de execução de seus códigos e verificar se os resultados estão de acordo com o esperado. Com o Gleam, essa ferramenta pode ser facilmente implementada e proporcionar uma melhor compreensão do funcionamento do programa.

## Como fazer:

A impressão de saída de depuração no Gleam é realizada com o uso do comando `debug!` seguido de uma expressão que se deseja imprimir. O resultado será mostrado no console durante a execução do programa.

```
Gleam
def contagem_inversa() {
    let numeros = [5, 4, 3, 2, 1]
    debug! numeros
    iterar(numeros)
}

def iterar(lista) {
    debug! "Iniciando iteração..."
    loop contador de 0 até comprimento(lista) - 1 {
        debug! "Iteração em andamento:", lista[contador]
    }
    debug! "Iteração concluída!"
}
```

A saída desse código seria:

```
[5, 4, 3, 2, 1]
Iniciando iteração...
Iteração em andamento: 5
Iteração em andamento: 4
Iteração em andamento: 3
Iteração em andamento: 2
Iteração em andamento: 1
Iteração concluída!
```

## Aprofundando:

Além de simplesmente imprimir valores, o comando `debug!` também pode ser usado para inserir condições em nossos programas, interrompendo a execução caso alguma dessas condições seja atendida. Isso pode ser útil para identificar possíveis erros e bugs no código.

Por exemplo, podemos adicionar um `debug!` dentro do loop `iterar` para imprimir apenas os valores pares da lista:

```
loop contador de 0 até comprimento(lista) - 1 {
    if lista[contador] % 2 == 0 {
        debug! "Número par encontrado:", lista[contador]
    }
}
```

A saída seria:

```
Número par encontrado: 4
Número par encontrado: 2
```

Vale ressaltar que é importante remover todos os comandos `debug!` antes de finalizar o código e colocá-los apenas quando necessário, caso contrário, pode haver um grande impacto na performance do programa.

# Veja também:

- Documentação oficial do Gleam sobre impressão de saída de depuração:
https://gleam.run/book/tour/side-effects.html#debugging-code

- Artigo sobre melhores práticas de debug em Gleam:
https://dev.to/thejunkland/dealing-with-debugging-in-production-with-gleam-1mer
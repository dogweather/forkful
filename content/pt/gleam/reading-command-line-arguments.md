---
title:                "Gleam: Lendo argumentos da linha de comando"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Por que?

Você pode estar se perguntando por que deveria se preocupar em aprender a ler argumentos de linha de comando em Gleam. Bem, a resposta é simples: a capacidade de ler argumentos de forma eficiente é uma habilidade fundamental para qualquer programador. Além disso, com o crescente uso de ferramentas de linha de comando no desenvolvimento de software, saber ler esses argumentos é uma vantagem significativa para qualquer desenvolvedor.

# Como Fazer

Agora que você está convencido de que é importante aprender a ler argumentos de linha de comando, vamos ao essencial. Vamos começar com um exemplo básico de como ler um argumento e imprimir seu valor na tela:

```Gleam
import gleam/options

fn main() {
  options := options.parse(gleam.options.Args)
  greeting := options.get("--greeting")
  io.print("Olá, " ++ greeting)
}
```

Neste exemplo, estamos usando a biblioteca de opções Gleam para facilitar a leitura dos argumentos. Primeiro, importamos a biblioteca com `import gleam/options`. Em seguida, usamos a função `parse` para criar uma lista de opções a partir dos argumentos fornecidos na linha de comando. Em seguida, usamos a função `get` para obter o argumento específico que desejamos, neste caso, `--greeting`. Finalmente, usamos a função `print` do módulo IO para imprimir o valor na tela.

Você pode testar este programa passando argumentos na linha de comando, por exemplo: `gleam run exemplo.gleam --greeting=Leitor`.

# Profundando

Agora que você tem uma noção básica de como ler argumentos de linha de comando em Gleam, vamos mergulhar um pouco mais fundo. Como mencionamos anteriormente, estamos usando a biblioteca de opções Gleam para facilitar a leitura de argumentos. Esta biblioteca nos fornece funções úteis como `parse`, `get` e `with_default`. Confira a documentação desta biblioteca para aprender mais sobre todas as funcionalidades disponíveis.

Além disso, essa biblioteca pode ser personalizada para atender às suas necessidades específicas. Você pode, por exemplo, criar suas próprias funções para lidar com argumentos específicos ou até mesmo criar sua própria versão da biblioteca com funcionalidades adicionais.

Outra coisa importante a se ter em mente é que a ordem dos argumentos na linha de comando é importante para que as funções de parsing funcionem corretamente. Portanto, é sempre bom ter uma estratégia para organizar seus argumentos de forma consistente.

# Veja também

- Documentação Gleam: https://gleam.run/documentation/
- Biblioteca de opções Gleam: https://github.com/gleam-lang/options
- Exemplos de código Gleam: https://github.com/search?q=topic%3Agleam+org%3Agleam-lang+type%3ARepositories
---
title:    "Elixir: Lendo argumentos da linha de comando"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Ao escrever um programa Elixir, pode ser útil ler argumentos da linha de comando para personalizar o comportamento do programa ou passar informações adicionais ao mesmo. Aprender a ler argumentos da linha de comando pode ajudar a tornar seus programas mais dinâmicos e flexíveis.

## Como fazer isso?

O Elixir possui uma biblioteca conveniente chamada `OptionParser` que nos permite ler e interpretar os argumentos da linha de comando. A seguir, estão alguns exemplos de códigos para ler e processar diferentes tipos de argumentos:

```
Elixir program.ex argumento1 argumento2
```
Este código irá passar os argumentos "argumento1" e "argumento2" para o programa e você pode acessá-los usando a função `OptionParser.parse/1`:

```
options = OptionParser.parse(System.argv)
```
Em seguida, você pode usar os argumentos lidos para personalizar o comportamento do programa ou fazer alguma operação específica. Aqui está um exemplo simples de um programa que recebe um argumento e imprime uma mensagem com ele:

```
defmodule MyProgram do
  def main do
    options = OptionParser.parse(System.argv)
    argument = OptionParser.get_argument(options, 0)
    IO.puts "Olá #{argument}!"
  end
end

MyProgram.main
```

Se você quiser acessar argumentos de flags, pode usar a opção `flag` no `OptionParser`:

```
options = OptionParser.parse(System.argv, flags: [:h])
flag_h = OptionParser.get_flag(options, :h)
```

Esta opção irá permitir que você execute seu programa com a flag `-h` para obter ajuda ou ver a lista de flags disponíveis.

## Profundidade

A biblioteca `OptionParser` também oferece mais recursos e opções para personalizar a leitura de argumentos da linha de comando. Você pode ler argumentos de diferentes tipos de dados, como strings, inteiros e booleanos, e até mesmo fazer validações para garantir que os argumentos estejam em um formato específico.

Além disso, a biblioteca também permite que você crie argumentos com valores padrão e argumentos opcionais, tornando sua leitura de argumentos ainda mais flexível e personalizada para suas necessidades.

## Veja também
- [Documentação oficial sobre a biblioteca OptionParser](https://hexdocs.pm/elixir/OptionParser.html)
- [Exemplo de leitura de argumentos da linha de comando usando OptionParser](https://elixirschool.com/pt/lessons/specifics/command-line-args/#patterns-operator)
- [Tutorial em vídeo sobre como ler argumentos da linha de comando com Elixir](https://www.youtube.com/watch?v=dKfrB2HbvMc)
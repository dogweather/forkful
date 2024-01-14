---
title:                "Elixir: Lendo argumentos da linha de comando"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando em Elixir?

Ler argumentos da linha de comando pode ser útil para obter informações específicas do usuário ou para fazer decisões baseadas nessas informações. Sempre que seu programa precisar de entrada externa, é importante saber como ler esses argumentos da maneira correta. Neste artigo, vamos explorar como ler argumentos da linha de comando em Elixir.

## Como ler argumentos da linha de comando em Elixir

Em Elixir, podemos ler argumentos da linha de comando usando o módulo [`OptionParser`](https://hexdocs.pm/elixir/OptionParser.html). Primeiro, precisamos importar o módulo em nosso código:

```
import OptionParser
```

Em seguida, podemos definir os argumentos que queremos ler usando a função [`OptionParser.on_parse/2`](https://hexdocs.pm/elixir/OptionParser.html#on_parse/2):

```
OptionParser.on_parse(:foo, "Description of argument foo", :required)
OptionParser.on_parse(:bar, "Description of argument bar", :optional)
```

Podemos especificar se o argumento é obrigatório ou opcional, bem como fornecer uma descrição para que o usuário saiba o que esperar. Em seguida, podemos definir os comandos que queremos que nosso programa aceite, usando a função [`OptionParser.parse/1`](https://hexdocs.pm/elixir/OptionParser.html#parse/1):

```
args = [foo: "some_value", bar: "another_value"]
parsed_args = OptionParser.parse(args)
```

Isso retornará um mapa com os argumentos lidos da linha de comando. Se um argumento opcional não for fornecido, ele terá um valor padrão de `nil`.

## Mergulho profundo

Uma vez que já sabemos como ler argumentos da linha de comando em Elixir, podemos nos aprofundar em algumas funcionalidades adicionais. Por exemplo, podemos usar a função [`OptionParser.help!/1`](https://hexdocs.pm/elixir/OptionParser.html#help!/1) para imprimir uma mensagem de ajuda com as descrições de todos os argumentos aceitos pelo nosso programa. Além disso, podemos até mesmo lidar com erros usando a função [`OptionParser.error/2`](https://hexdocs.pm/elixir/OptionParser.html#error/2) para personalizar mensagens de erro.

## Veja também

- [Documentação do módulo OptionParser](https://hexdocs.pm/elixir/OptionParser.html)
- [Tutorial de programação em Elixir para iniciantes](https://medium.com/@gabirabelo/elixir-primeiros-passos-4ccda5908dab)
- [Comunidade Elixir Brasil](https://elixir-brasil.org/)
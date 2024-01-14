---
title:    "Elm: Escrevendo um arquivo de texto"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto em Elm?

Escrever um arquivo de texto em Elm pode ser útil para armazenar informações, configurações ou dados que precisem ser acessados e modificados no futuro. Além disso, a escrita de arquivos de texto pode ser usada para gerar documentos ou relatórios em formato de texto simples.

## Como fazer?

Para começar, precisaremos importar o módulo `File` do pacote `elm/file`.

```Elm
import File
```

Em seguida, vamos utilizar a função `File.writeString` para escrever em um arquivo de texto.

```Elm
main =
  File.writeString "meu_arquivo.txt" "Este é um texto de exemplo."
```

O primeiro argumento de `File.writeString` é o nome do arquivo que desejamos criar ou modificar. O segundo argumento é o conteúdo que queremos escrever no arquivo.

Após executar o código acima, um novo arquivo chamado "meu_arquivo.txt" será criado e o texto `"Este é um texto de exemplo."` será escrito dentro dele.

## Aprofundando-se

Além da função `File.writeString`, o módulo `File` possui outras funções úteis para trabalhar com arquivos de texto, como `File.read`, que retorna o conteúdo de um arquivo, e `File.append`, que adiciona conteúdo a um arquivo preexistente.

É importante lembrar que o pacote `elm/file` só pode ser utilizado em aplicações compiladas (não funciona no Elm REPL ou no navegador), e que os arquivos escritos por um programa Elm só podem ser acessados pelo próprio programa.

## Veja também

- Documentação do módulo `File`: https://package.elm-lang.org/packages/elm/file/latest/File
- Tutorial sobre I/O em Elm: https://www.elm-tutorial.org/en/09-persistence/01-intro.html
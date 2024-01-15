---
title:                "Escrevendo um arquivo de texto"
html_title:           "Haskell: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Escrever um arquivo de texto pode ser uma necessidade em muitos projetos de programação. Isso pode ser útil para salvar informações, criar relatórios ou armazenar dados temporários. Além disso, é uma habilidade importante para qualquer pessoa que deseje se tornar um programador melhor.

## Como Fazer

Escrever um arquivo de texto em Haskell é simples e pode ser feito em poucas etapas.

Primeiro, importe o módulo System.IO usando o comando `import System.IO`. Este módulo fornece funções para lidar com arquivos.

Em seguida, crie uma função principal que irá escrever o arquivo de texto. Você pode nomeá-la como quiser, mas vamos chamar de `escreverArquivo` para fins de exemplo. A função deve ter tipo `IO ()`, indicando que ela executa ações do tipo IO.

Dentro da função `escreverArquivo`, crie uma variável que armazenará o caminho e o nome do arquivo que desejamos escrever. Vamos nomeá-la de `destino` e atribuir o caminho para o nosso arquivo de texto. Por exemplo: `let destino = "C:\\Users\\Usuario\\Desktop\\meu_arquivo.txt"`.

Em seguida, use o comando `writeFile` para criar o arquivo. Ele recebe dois parâmetros: o caminho e o conteúdo que desejamos escrever. No nosso caso, vamos criar uma mensagem simples de "Olá mundo!" e salvá-la no arquivo. O código ficaria assim: `writeFile destino "Olá mundo!"`.

Por fim, basta chamar a função `escreverArquivo` dentro da função principal e executar o programa. O arquivo será criado no caminho especificado com o conteúdo que definimos.

Veja um exemplo completo:

```haskell
import System.IO

-- função principal
main = do
    escreverArquivo

-- função que escreve um arquivo de texto
escreverArquivo :: IO ()
escreverArquivo = do
    let destino = "C:\\Users\\Usuario\\Desktop\\meu_arquivo.txt"
    writeFile destino "Olá mundo!"
```

Ao executar esse código, o arquivo "meu_arquivo.txt" será criado na área de trabalho (ou no caminho especificado) com o conteúdo "Olá mundo!".

## Aprofundando

Existem outras funções disponíveis no módulo System.IO que podem ser úteis ao trabalhar com arquivos de texto em Haskell. Aqui estão algumas delas:

- `appendFile`: essa função é semelhante ao `writeFile`, mas ela adiciona o conteúdo ao final do arquivo, ao invés de substituí-lo.
- `getContents`: essa função lê todo o conteúdo de um arquivo e o retorna como uma string.
- `hPutStrLn`: essa função é similar ao `putStrLn` que vimos no artigo sobre input de dados, mas ela escreve a string no arquivo especificado.
- `readFile`: essa função lê todo o conteúdo de um arquivo e o retorna como uma string.

Além disso, é importante ter em mente algumas práticas recomendadas ao trabalhar com arquivos de texto em Haskell:

- Lembre-se de fechar o arquivo após usá-lo. Isso pode ser feito usando a função `hClose`.
- Verifique se o arquivo existe antes de tentar lê-lo ou escrevê-lo. Caso contrário, pode ocorrer um erro.
- Certifique-se de que o arquivo esteja em um formato compatível com o que você deseja ler ou escrever. É possível usar funções como `read` e `show` para converter tipos de dados em strings e vice-versa.

## Veja também

- [Documentação do módulo System.IO](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-IO.html)
- [Tutorial de Haskell](https://www.tutorialspoint.com/haskell/index.htm)
- [Haskell Para Você](https://haskellparavoce.ninja/)
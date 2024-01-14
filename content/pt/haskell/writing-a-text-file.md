---
title:    "Haskell: Escrevendo um arquivo de texto"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto em Haskell?

Escrever um arquivo de texto em Haskell pode ser útil para armazenar dados ou resultados de um programa de forma permanente. Além disso, é uma habilidade essencial para programas que lidam com I/O (entrada e saída), pois permite que o usuário interaja com o programa através de um arquivo de texto.

## Como fazer:

Para escrever um arquivo de texto em Haskell, é necessário importar o módulo "System.IO". Em seguida, utilizamos a função "writeFile" para escrever o conteúdo desejado no arquivo, passando como argumento o nome do arquivo e o conteúdo. Por exemplo:

```Haskell
import System.IO

main = do
    writeFile "meuarquivo.txt" "Olá, mundo!"
```

**Saída:** Um arquivo chamado "meuarquivo.txt" será criado com o conteúdo "Olá, mundo!". É importante lembrar que a função "writeFile" sobrescreve o conteúdo de um arquivo existente, portanto é importante tomar cuidado caso não queira perder informações.

Outra forma de escrever em um arquivo é utilizar a função "appendFile", que acrescenta conteúdo no final do arquivo em vez de sobrescrevê-lo.

Para ler o conteúdo de um arquivo de texto, podemos utilizar a função "readFile", que retorna uma string com o conteúdo do arquivo. Exemplo:

```Haskell
main = do
    conteudo <- readFile "meuarquivo.txt"
    putStrLn conteudo
```

**Saída:** O conteúdo do arquivo "meuarquivo.txt" será exibido no terminal.

## Mergulho profundo:

Além das funções "writeFile", "appendFile" e "readFile", o módulo "System.IO" possui diversas outras funções para lidar com arquivos de texto em Haskell. É possível abrir, fechar, renomear e mover arquivos, além de realizar operações de I/O com arquivos binários.

Lembrando que em Haskell, a manipulação de arquivos é feita de forma pura, ou seja, não há efeitos colaterais e a função sempre retornará o mesmo resultado para os mesmos argumentos. Isso torna o código mais seguro e fácil de testar.

## Veja também:

- [Documentação do módulo System.IO](https://www.haskell.org/onlinereport/haskell2010/haskellch9.html)
- [Tutorial de Haskell na Wikibooks](https://pt.wikibooks.org/wiki/Haskell/Exemplo_de_programação_com_IO_avançado)
- [Guia de Haskell no Wikiversity](https://pt.wikiversity.org/wiki/Guia_de_Haskell,_Parte_6:_Entrada_e_saída)
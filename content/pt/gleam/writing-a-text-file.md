---
title:                "Gleam: Escrevendo um arquivo de texto"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

##Por que

Escrever um arquivo de texto é uma tarefa essencial para qualquer programador, especialmente se você está trabalhando com a linguagem de programação Gleam. Isso permite que você armazene informações importantes e organize o seu código de forma mais eficiente.

##Como Fazer

Para escrever um arquivo de texto em Gleam, você pode seguir os seguintes passos:

1. Primeiro, crie um novo projeto Gleam ou acesse um já existente.
2. Importe a biblioteca "File".
3. Abra o arquivo com a função "File.write" e atribua um nome ao arquivo.
4. Use a função "File.close" para fechar o arquivo após terminar de escrever.
5. Use a função "File.writeln" para escrever linhas no arquivo.

Aqui está um exemplo de código em Gleam para escrever um arquivo de texto:

```Gleam
import gleam/file

let file = File.write("texto.txt")
File.writeln(file, "Olá, mundo!")
File.writeln(file, "Como você está?")
File.close(file)
```

O arquivo resultante "texto.txt" terá o seguinte conteúdo:

```
Olá, mundo!
Como você está?
```

##Mergulho Profundo

Existem várias opções que podem ser passadas como argumentos nas funções "File.write" e "File.writeln" para personalizar o seu arquivo de texto. Aqui estão alguns exemplos:

- Para adicionar uma quebra de linha personalizada, você pode usar a opção "end" e especificar um caractere, como "\n" ou "\r".
- Para escrever no início do arquivo, você pode usar a opção "start" e definir como "true".
- Você pode especificar a codificação do arquivo com a opção "encoding". Por padrão, é usada a codificação UTF-8.

Além disso, você pode usar outras funções fornecidas pela biblioteca "File" para ler e manipular arquivos de texto, como "File.read" e "File.contains".

##Veja Também

- Documentação da biblioteca "File" em Gleam: https://gleam.run/modules/gleam/file.html
- Tutorial de Gleam: https://gleam.run/tutorials
- Livro de Gleam: https://shop.miramicoding.com/products/gleam-programming-language-book
---
title:    "Gleam: Lendo um arquivo de texto."
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Ler arquivos de texto é uma tarefa básica em muitos programas, especialmente ao lidar com entrada de dados e geração de relatórios. Ao aprender a ler um arquivo de texto em Gleam, você poderá expandir suas habilidades de programação e criar aplicativos mais robustos e dinâmicos.

## Como fazer:

Para ler um arquivo de texto em Gleam, você precisa seguir estes passos:

1. Abra o arquivo usando a função `File.open()` e passe o caminho do arquivo como argumento:
   ```
   let file = File.open("caminho/do/arquivo.txt")
   ```
2. Leia o conteúdo do arquivo usando o método `read()`:
   ```
   let content = file.read()
   ```
3. Feche o arquivo usando o método `close()`:
   ```
   file.close()
   ```
4. Faça o parse do conteúdo do arquivo usando as funções apropriadas, como `String.split()` ou `String.to_int()`.

Aqui está um exemplo completo de como ler um arquivo de texto e imprimir seu conteúdo:

```
Gleam import String

let file = File.open("caminho/do/arquivo.txt")
let content = file.read()
file.close()

let lines = String.split(content, "\n")
for line in lines {
  let words = String.split(line, " ")
  for word in words {
    println(word)
  }
}
```

A saída deste exemplo será uma lista de todas as palavras encontradas no arquivo.

## Mergulho profundo:

Ao ler um arquivo de texto em Gleam, é importante levar em consideração o tamanho e o formato do arquivo. Um arquivo muito grande pode afetar o desempenho do seu aplicativo, por isso é importante usar métodos como `File.read_chunk()` ao lidar com arquivos grandes. Além disso, é possível também especificar o modo de leitura do arquivo ao abri-lo, como `File.open("caminho/do/arquivo.txt", "r+")` se você precisar ler e escrever no arquivo.

Outro ponto importante a se considerar é o encoding do arquivo. Certifique-se de usar o método correto ao fazer o parse do conteúdo do arquivo, como `String.to_utf8()` ou `String.to_bytes()`.

## Veja também:

- [Documentação oficial do módulo File em Gleam (em inglês)](https://gleam.run/modules/standard-lib/file.html)
- [Tutorial de leitura de arquivos em Gleam (em inglês)](https://blog.gleam.run/reading-files-in-gleam/)
- [Exemplos práticos de uso de arquivos em Gleam (em inglês)](https://github.com/gleam-lang/gleam/blob/master/examples/files/src/files.gleam)
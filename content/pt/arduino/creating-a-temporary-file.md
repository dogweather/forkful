---
title:                "Criando um arquivo temporário"
html_title:           "Arduino: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O que & Porquê?
Criar um arquivo temporário é simplesmente criar um arquivo que é usado apenas temporariamente e excluído quando já não é necessário. Os programadores geralmente criam arquivos temporários para armazenar dados temporários ou para fins de teste.

## Como fazer:
```
Arduino File tmpFile = File.createTempFile("Temp", ".txt"); // cria um arquivo temporário
tmpFile.write("Este é um arquivo temporário."); // escreve o conteúdo no arquivo
tmpFile.read(); // lê o arquivo
tmpFile.delete(); // exclui o arquivo
```

## Mergulho Profundo:
Criar arquivos temporários se tornou uma prática comum entre os programadores por ser uma maneira conveniente de lidar com dados temporários sem precisar criar um arquivo permanente. Existem também outras alternativas, como o uso de memória temporária ou variáveis, mas a criação de arquivos temporários permite uma maior flexibilidade e organização dos dados.

Além disso, ao criar um arquivo temporário, o programador pode especificar o local e o nome do arquivo, permitindo assim um maior controle sobre os dados temporários. A implementação de criação de arquivos temporários varia de acordo com a linguagem de programação e o sistema operacional utilizado, mas geralmente é simples e fácil de utilizar.

## Veja também:
- [Artigo sobre criação de arquivos temporários em Java](https://www.baeldung.com/java-temporary-files)
- [Documentação oficial sobre arquivos temporários em Arduino](https://www.arduino.cc/reference/en/language/functions/files-and-dirs/file/createtempfile/)
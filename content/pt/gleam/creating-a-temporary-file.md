---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Lidando com Arquivos Temporários em Gleam: Um Guia Vegetativo

## O Que & Por Quê?
Criar um arquivo temporário significa estabelecer um local para armazenar dados de curta duração. Fazemos isso quando precisamos usar esses dados durante uma sessão de programa e depois descartá-los sem deixar qualquer pegada persistente.

## Como Fazer:
Vamos dividir isso em duas seções. Primeiro, criação e escrita para um arquivo temporário. Segundo, leitura e exclusão deste arquivo.

```gleam
import gleam/otp/process
import gleam/io

fn main() {
    let name = io.tmpfile().unwrap()
    io.write_file(name, "Trabalhando com arquivos temporários em Gleam.")
    io.println(name)
}
```
Este código cria um arquivo temporário, escreve um string nele, e então imprime o nome do arquivo. Agora vamos ler e excluir o arquivo.

```gleam
import gleam/otp/process
import gleam/io

fn read_and_delete(filename: String) {
    let content = io.read_file(filename).unwrap()
    io.println(content)
    io.delete_file(filename)
}
```
Deve ser bem direto - leia o arquivo, gratifique-se com a magia impressa na tela e limpe depois que terminar.

## Mergulho Profundo
Trabalhar com arquivos temporários tem sido uma parte fundamental da programação desde o início dos dias computacionais. Por que? Porque a memória não-volátil era (e ainda é) um recurso valioso e o espaço de armazenamento era (e algumas vezes, ainda é) limitado.

No Gleam, os arquivos temporários são manipulados em uma base por-sessão. Isso significa que eles são criados no início de uma sessão de programa e removidos automaticamente no final dela, a não ser que você o faça explicitamente (como fizemos no exemplo acima).

Em termos de alternativas, poderíamos ter usado datastores em memória (como Redis) ou mesmo uma base de dados normal se nossos dados tivessem sido mais persistentes.

## Veja Também
- Documentação oficial de Gleam IO: https://hexdocs.pm/gleam/io/
- Aplicações práticas de arquivos temporários: https://betterprogramming.pub/why-and-how-to-use-temporary-files-in-your-programs-8207ec601d6e

Basta intercalar essas duas pequenas funções no seu código e você terá os arquivos temporários sob controle!
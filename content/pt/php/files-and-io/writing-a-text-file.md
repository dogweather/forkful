---
title:                "Escrevendo um arquivo de texto"
aliases:
- /pt/php/writing-a-text-file.md
date:                  2024-02-03T19:28:44.222670-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escrevendo um arquivo de texto"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?
Escrever um arquivo de texto em PHP envolve criar ou abrir um arquivo e inserir conteúdo nele. Programadores fazem isso para persistir dados, como conteúdo gerado pelo usuário ou registros, além do ciclo de vida do programa.

## Como fazer:
O PHP suporta nativamente a escrita de arquivos por meio de funções como `file_put_contents`, `fopen` juntamente com `fwrite` e `fclose`. Veja como usá-las:

### Escrevendo Simplesmente com `file_put_contents`:
Esta função simplifica o processo de escrever em um arquivo fazendo tudo em um passo só.
```php
$content = "Olá, mundo!";
file_put_contents("hello.txt", $content);
// Verifica se o arquivo foi escrito com sucesso
if (file_exists("hello.txt")) {
    echo "Arquivo criado com sucesso!";
} else {
    echo "Falha ao criar o arquivo.";
}
```

### Escrita Avançada com `fopen`, `fwrite` e `fclose`:
Para mais controle sobre a escrita do arquivo, como adicionar texto ou mais tratamento de erros, use `fopen` com `fwrite`.
```php
$file = fopen("hello.txt", "a"); // modo 'a' para adicionar, 'w' para escrever
if ($file) {
    fwrite($file, "\nAdicionando mais conteúdo.");
    fclose($file);
    echo "Conteúdo adicionado com sucesso!";
} else {
    echo "Falha ao abrir o arquivo.";
}
```

#### Lendo o Arquivo para Saída:
Para verificar nosso conteúdo:
```php
echo file_get_contents("hello.txt");
```
**Saída de Exemplo:**
```
Olá, mundo!
Adicionando mais conteúdo.
```

### Usando Bibliotecas de Terceiros:
Para operações de arquivo mais complexas, bibliotecas como `League\Flysystem` podem ser usadas para uma camada de abstração sobre o sistema de arquivos, mas as funções incorporadas do PHP são frequentemente suficientes para tarefas básicas de escrita de arquivos. Aqui está um breve exemplo se você optar por explorar o `Flysystem`:
```php
require 'vendor/autoload.php';
use League\Flysystem\Filesystem;
use League\Flysystem\Local\LocalFilesystemAdapter;

$adapter = new LocalFilesystemAdapter(__DIR__);
$filesystem = new Filesystem($adapter);

$filesystem->write('hello.txt', "Usando Flysystem para escrever isto.");
```
Este exemplo assume que você instalou o `league/flysystem` via Composer. Bibliotecas de terceiros podem simplificar bastante o manuseio de arquivos mais complexos, especialmente ao trabalhar com diferentes sistemas de armazenamento de forma integrada.

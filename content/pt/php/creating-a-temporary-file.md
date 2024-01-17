---
title:                "Criando um arquivo temporário"
html_title:           "PHP: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O que é e Porquê?
Criar um arquivo temporário é uma técnica utilizada por programadores para armazenar dados temporariamente durante a execução de um programa em PHP. Isso é útil para evitar a sobrecarga de memória e garantir um melhor desempenho do código.

## Como fazer:
Existem diferentes maneiras de criar um arquivo temporário em PHP, dependendo das suas necessidades. Aqui estão dois exemplos de código:

```
<?php
// Usando a função tmpfile ()
$arquivo = tmpfile();
fwrite($arquivo, "Este é um arquivo temporário.");
// Retorna um ponteiro para o arquivo temporário
echo ftell($arquivo); // imprime 30

// Usando a função tempnam ()
$arquivo = tempnam("/tmp", "temp");
file_put_contents($arquivo, "Este é um arquivo temporário.");
echo file_get_contents($arquivo); // imprime "Este é um arquivo temporário."
?>
```

## Mergulho Profundo:
Criar arquivos temporários é uma prática comum em programação e já é amplamente suportada em várias linguagens de programação. No PHP, existem várias funções que podem ser usadas para isso, como as mencionadas acima. Além disso, você também pode optar por criar seus próprios métodos para gerenciar arquivos temporários, dependendo das suas necessidades específicas.

## Veja também:
- Documentação oficial do PHP sobre criação de arquivos temporários: https://www.php.net/manual/pt_BR/function.tmpfile.php
- Tutorial online sobre a criação de arquivos temporários com PHP: https://www.tutorialspoint.com/create-a-temporary-file-in-php
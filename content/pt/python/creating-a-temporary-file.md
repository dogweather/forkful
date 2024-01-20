---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O quê & Por quê?

Criar um arquivo temporário é um processo onde um arquivo apenas para uso momentâneo é gerado por um programa. Programadores fazem isso para armazenar dados transientes que não são necessários após o uso do programa ou em situações em que o armazenamento permanente seria ineficiente.

## Como fazer:

Aqui está um exemplo simples de como criar um arquivo temporário usando a biblioteca `tempfile` no Python:

```python
import tempfile

temp = tempfile.TemporaryFile()
print(temp)
print(temp.name)
```

Ao executar este código, você receberá um resultado assim:

```
<_io.BufferedRandom name=3>
/tmp/tmpkj7tdk3y
```

Isto mostra que um arquivo temporário foi criado na localização exibida.

## Mergulho Profundo

Na era inicial da computação, o espaço de armazenamento era precioso e caro. Por isso, o conceito de arquivos temporários nasceu como uma maneira de economizar no armazenamento. Hoje, mesmo com o abundante espaço de armazenamento, os arquivos temporários continuam sendo uma prática comum para gerenciar dados transientes de maneira eficiente.

Uma alternativa ao uso de arquivos temporários seria manter os dados em memória do seu programa. No entanto, para grandes volumes de dados, isso não é ideal. Há também as opções de usar bancos de dados ou outros meios de armazenamento persistente, mas esses são geralmente mais demorados e complicados.

Ao criar um arquivo temporário no Python usando `tempfile`, você está não está apenas criando um arquivo em seu sistema operacional, mas também configurando-o para remoção automática quando ele for fechado. Adicionalmente, `tempfile` gerencia o arquivo de maneira segura, garantindo que não haja conflitos de nomes de arquivo ou questões de acessibilidade.

## Ver Também

Para mais detalhes, confira a documentação oficial do Python sobre a biblioteca `tempfile`: https://docs.python.org/3/library/tempfile.html

Se você precisar de mais informações sobre gerenciamento de arquivos temporários em sistemas operacionais em geral, esta leitura pode ser útil: https://en.wikipedia.org/wiki/Temporary_folder

E aqui está uma discussão StackOverflow sobre quando usar arquivos temporários versus manter os dados em memória: https://stackoverflow.com/questions/788411/check-if-a-python-list-item-contains-a-string-inside-another-string
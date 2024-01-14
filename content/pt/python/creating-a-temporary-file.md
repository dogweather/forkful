---
title:    "Python: Criando um arquivo temporário"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por que criar arquivos temporários em Python

Se você está pensando em criar um arquivo temporário em Python, provavelmente está se perguntando qual é a vantagem disso. Afinal, por que adicionar essa etapa extra no seu código? Bem, existem algumas razões pelas quais criar um arquivo temporário pode ser útil.

Criar um arquivo temporário é útil quando você precisa armazenar dados temporariamente durante a execução de um programa. Isso pode ser especialmente útil quando você está trabalhando com grandes quantidades de dados e quer evitar sobrecarregar a memória do seu computador. Ao criar um arquivo temporário, você pode armazenar esses dados temporários e, em seguida, excluí-los quando não for mais necessário. Isso também pode ser útil para evitar conflitos entre diferentes processos que estão sendo executados simultaneamente no seu computador.

## Como criar um arquivo temporário em Python

Criar um arquivo temporário em Python é bastante simples. Você só precisa importar o módulo "tempfile" e usar a função "NamedTemporaryFile()". Veja um exemplo:

```Python
import tempfile

#Criar arquivo temporário
with tempfile.NamedTemporaryFile() as tmp:
    # Escrever dados no arquivo
    tmp.write(b"Informações de exemplo")
    # Ler dados do arquivo
    print(tmp.read())
```

Esse código irá criar um arquivo temporário automaticamente e armazená-lo na variável "tmp". Você pode então usar o arquivo temporário como faria com qualquer outro arquivo em Python, gravando e lendo dados nele. Lembre-se de que, ao usar a função "NamedTemporaryFile()", você está criando um novo arquivo temporário que será excluído automaticamente quando o processo for encerrado.

## Mergulho profundo na criação de arquivos temporários em Python

Embora o exemplo acima seja um bom ponto de partida para criar arquivos temporários em Python, existem algumas outras coisas que você deve saber. Por exemplo, se você precisar criar vários arquivos temporários no seu código, pode ser útil definir um diretório padrão para eles usando a função "tempfile.tempdir". Além disso, você também pode especificar o modo de abertura do arquivo temporário, assim como faria com qualquer outro arquivo em Python.

Se você quiser ser ainda mais específico, pode usar a função "mktemp()" para criar um arquivo temporário com um nome personalizado. Isso pode ser útil quando você precisa acessar o arquivo temporário por meio de outro processo ou programa.

Lembre-se de que, embora os arquivos temporários sejam automaticamente excluídos quando o processo é encerrado, você também pode excluí-los manualmente usando a função "tmp.close()".

## Veja também

Aqui estão alguns links úteis para saber mais sobre a criação de arquivos temporários em Python:

- Documentação oficial do módulo "tempfile": https://docs.python.org/3/library/tempfile.html
- Tutorial sobre a criação de arquivos temporários em Python: https://www.geeksforgeeks.org/python-create-temporary-files-and-directories/
- Exemplos de uso de arquivos temporários em Python: https://www.tutorialspoint.com/python/python_temporary_files.htm
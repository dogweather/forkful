---
title:    "Python: Criando um arquivo temporário"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Porquê

Criar um arquivo temporário é uma tarefa comum na programação. Ele permite que você armazene dados temporários que não são relevantes para o seu programa principal. Além disso, eles são automaticamente excluídos quando o programa termina, evitando a sobrecarga de armazenamento desnecessário.

## Como Fazer

Criar um arquivo temporário em Python é muito simples. Basta seguir estes passos:

```Python
import tempfile

# Criando um arquivo temporário
temp_file = tempfile.NamedTemporaryFile()

# Escrevendo dados no arquivo
temp_file.write("Olá mundo!")

# Lendo dados do arquivo
temp_file.seek(0)
print(temp_file.read())

# Fechando o arquivo
temp_file.close()
```

Saída: Olá mundo!

Neste exemplo, importamos o módulo "tempfile" e criamos um arquivo temporário chamado "temp_file" utilizando a função "NamedTemporaryFile()". Em seguida, escrevemos a frase "Olá mundo!" no arquivo e lemos o seu conteúdo utilizando a função "read()". Por fim, fechamos o arquivo para garantir que ele seja excluído após o programa ser encerrado.

## Mergulho Profundo

Ao criar um arquivo temporário, é possível especificar alguns parâmetros como o modo (leitura, escrita ou ambos), o sufixo e o prefixo do arquivo. Além disso, é importante lembrar de fechar o arquivo quando ele não for mais necessário, pois isso garantirá que ele seja excluído corretamente.

## Veja Também

- [Documentação oficial do módulo "tempfile" em Python](https://docs.python.org/3/library/tempfile.html)
- [Tutorial sobre arquivos temporários em Python](https://www.tutorialspoint.com/python3/python_files_io.htm)
- [Artigo sobre como usar arquivos temporários em Python](https://realpython.com/working-with-files-in-python/)
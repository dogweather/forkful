---
title:                "Criando um arquivo temporário"
html_title:           "Python: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Python?

Existem diversas razões pelas quais alguém pode precisar criar um arquivo temporário em Python. Algumas delas incluem guardar informações temporárias, compartilhar dados entre diferentes programas ou verificar se um determinado arquivo é válido antes de salvá-lo permanentemente.

## Como criar um arquivo temporário em Python

Para criar um arquivo temporário em Python, podemos usar o módulo `tempfile`. Veja um exemplo simples abaixo:

```Python
import tempfile

# Cria um arquivo temporário
arquivo_temp = tempfile.NamedTemporaryFile()

# Escreve uma mensagem no arquivo
arquivo_temp.write(b"Olá, mundo!")

# Lê o conteúdo do arquivo
arquivo_temp.seek(0)
conteudo = arquivo_temp.read()

# Imprime a mensagem
print(conteudo)

# Fecha o arquivo temporário
arquivo_temp.close()
```

Ao executar o código acima, você deverá ver a mensagem "Olá, mundo!" impressa no console. É importante lembrar que arquivos temporários são criados em locais temporários do sistema operacional e serão automaticamente apagados ao fechá-los.

## Aprofundando-se em arquivos temporários

Além de criar arquivos temporários sem nome, como no exemplo acima, o módulo `tempfile` também nos permite criar arquivos temporários com um nome específico, navegar em diretórios temporários e até mesmo criar diretórios temporários. Para mais informações, recomendo a leitura da documentação oficial do módulo.

## Veja também

- Documentação oficial do módulo `tempfile`: https://docs.python.org/3/library/tempfile.html
- Tutorial sobre arquivos temporários em Python: https://realpython.com/python-tempfile/
- Mais opções para gerenciamento de arquivos em Python: https://www.pythonforbeginners.com/files/reading-and-writing-files-in-python
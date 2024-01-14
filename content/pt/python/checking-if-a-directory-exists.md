---
title:                "Python: Verificando se um diretório existe"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe

Ao escrever código em Python, muitas vezes você se depara com situações em que precisa verificar se um determinado diretório existe ou não. Isso é importante para garantir a integridade e o funcionamento adequado do seu código. Neste post, vamos explicar por que é importante verificar se um diretório existe e como fazer isso em Python.

## Como verificar se um diretório existe

Para verificar se um diretório existe em Python, você pode usar o método `os.path.exists()`. Este método recebe um caminho como argumento e retorna `True` se o diretório existir ou `False` se não existir. Veja um exemplo de código abaixo:

```Python
import os

diretorio = "Caminho/para/meu/diretorio"

if os.path.exists(diretorio):
    print("O diretório existe!")
else:
    print("O diretório não existe.")
```

Se o diretório existir, o output será: `O diretório existe!`. Caso contrário, o output será: `O diretório não existe.`. É importante notar que o caminho fornecido pode ser absoluto (o caminho completo até o diretório) ou relativo (o caminho a partir do diretório atual).

## Deep Dive em verificar se um diretório existe

O método `os.path.exists()` na verdade chama outra função interna chamada `os.stat()`. Esta função retorna um objeto `os.stat_result` que contém várias informações sobre o arquivo ou diretório, incluindo o tipo de arquivo, tamanho e data de modificação. Caso o caminho fornecido não exista, uma exceção `FileNotFoundError` será gerada.

## Veja também

- Documentação oficial do Python para o módulo `os.path`: https://docs.python.org/3/library/os.path.html
- Guia completo sobre trabalhar com arquivos e diretórios em Python: https://realpython.com/working-with-files-in-python/
- Exemplos práticos de como verificar se um diretório existe em diferentes cenários: https://www.oreilly.com/library/view/python-cookbook-3rd/9781449357337/ch04s03.html
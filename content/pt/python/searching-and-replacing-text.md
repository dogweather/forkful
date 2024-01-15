---
title:                "Buscando e substituindo texto"
html_title:           "Python: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que
Você já se encontrou em uma situação onde precisava alterar o texto em vários arquivos de uma vez? A funcionalidade de busca e substituição de texto é uma ferramenta poderosa que pode simplificar esse tipo de tarefa e economizar tempo e esforço.

## Como fazer
Usando Python, é possível realizar busca e substituição de texto de forma rápida e eficiente. Veja abaixo um exemplo de como fazer isso:

```Python
import os

# Definindo o diretório onde os arquivos serão alterados
diretorio = "meus_arquivos"

# Definindo o termo que será buscado e substituído
termo_busca = "texto antigo"
termo_substituir = "texto novo"

# Loop através de todos os arquivos no diretório
for arquivo in os.listdir(diretorio):
    # Abre o arquivo e lê o conteúdo
    with open(os.path.join(diretorio, arquivo)) as f:
        conteudo = f.read()
    # Faz a substituição do texto desejado
    conteudo = conteudo.replace(termo_busca, termo_substituir)
    # Sobrescreve o arquivo com o conteúdo atualizado
    with open(os.path.join(diretorio, arquivo), "w") as f:
        f.write(conteudo)
```

Ao executar este código, serão percorridos todos os arquivos dentro do diretório especificado e o termo "texto antigo" será substituído por "texto novo". O resultado final será a alteração do conteúdo desses arquivos.

## Aprofundando
Além de substituir um termo específico, é possível utilizar expressões regulares para realizar buscas mais complexas e substituições inteligentes. As expressões regulares permitem que você procure padrões de texto, o que pode ser útil em casos como formatação de data, números de telefone, endereços de email, entre outros.

Veja um exemplo de como utilizar expressões regulares com a biblioteca "re" do Python:

```Python
import re

# Definindo um padrão para procurar números de telefone
padrao = r"\d{2}-\d{4}-\d{4}"

# Definindo um texto de exemplo
texto = "Meu número de telefone é 11-4752-9843"

# Utilizando a expressão regular para encontrar o número de telefone
resultado = re.search(padrao, texto)

# Imprimindo o número de telefone encontrado
print(resultado.group())
```

Neste caso, o resultado final será "11-4752-9843", já que é o único número de telefone que segue o padrão definido na expressão regular.

## Veja também
- [Documentação oficial do Python sobre expressões regulares](https://docs.python.org/pt-br/3/library/re.html)
- [Tutorial do Real Python sobre busca e substituição de texto](https://realpython.com/search-and-replace-python/)
- [Guia do Python para iniciantes](https://www.python.org/about/gettingstarted/)
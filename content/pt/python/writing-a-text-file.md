---
title:    "Python: Escrevendo um arquivo de texto"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por Que

Escrever um arquivo de texto é uma habilidade essencial para qualquer programador Python. Ele permite armazenar dados de forma organizada e acessá-los facilmente para processamento posterior. Além disso, é uma maneira eficaz de salvar e compartilhar informações com outras pessoas.

## Como Fazer

Aqui está um exemplo de código Python para escrever um arquivo de texto usando a função `open()`:

```Python
arquivo = open("meu_arquivo.txt", "w") 
# "w" significa que o arquivo será aberto em modo de escrita

texto = "Olá, mundo!" # texto que será escrito no arquivo

arquivo.write(texto) # escreve o texto no arquivo
arquivo.close() # fecha o arquivo
```

O código acima criará um arquivo chamado "meu_arquivo.txt" e escreverá a frase "Olá, mundo!" dentro dele. Você também pode adicionar várias linhas de texto usando a função `write()` várias vezes antes de fechar o arquivo.

Outro exemplo usando a estrutura de controle `with` para garantir que o arquivo seja fechado automaticamente após a conclusão:

```Python
with open("meu_arquivo.txt", "w") as arquivo:
    arquivo.write("Essa é uma linha.\n") # \n é usado para adicionar uma quebra de linha
    arquivo.write("Essa é outra linha.")
```

O resultado será um arquivo com duas linhas: "Essa é uma linha." e "Essa é outra linha.".

## Mergulho Profundo

Além de escrever texto simples, você também pode formatar o texto em seu arquivo usando a sintaxe de formatação do Python. Por exemplo, você pode usar as função `format()` para inserir variáveis em seu texto de forma dinâmica:

```Python
nome = "Maria"
idade = 25

with open("meu_arquivo.txt", "w") as arquivo:
    texto = "Olá, meu nome é {} e tenho {} anos.".format(nome, idade)
    arquivo.write(texto)
```

O resultado será: "Olá, meu nome é Maria e tenho 25 anos.".

Você também pode usar a sintaxe de string multilinha para escrever várias linhas de texto de uma vez:

```Python
compras = ["maçãs", "bananas", "laranjas"]

with open("minhas_compras.txt", "w") as arquivo:
    texto = """Minha lista de compras:
    - {}
    - {}
    - {}""".format(*compras) # o operador * é usado para expandir a lista como argumentos separados
    arquivo.write(texto)
```

O resultado será um arquivo com três linhas listando suas compras.

## Veja também

- [Documentação oficial do Python sobre leitura e escrita de arquivos](https://docs.python.org/pt-br/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutorial de formatação de strings em Python](https://www.digitalocean.com/community/tutorials/how-to-use-string-formatters-in-python-3)
- [Guia completo sobre manipulação de arquivos em Python](https://realpython.com/read-write-files-python/)
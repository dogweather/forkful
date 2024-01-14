---
title:    "Python: Lendo argumentos da linha de comando"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Por que ler argumentos da linha de comando em Python?

Ler argumentos da linha de comando em Python é uma habilidade essencial para qualquer programador. Isso permite que você crie programas mais interativos e versáteis, que podem ser executados com diferentes entradas a partir da linha de comando. Além disso, dominar a leitura de argumentos da linha de comando é um sinal de proficiência em Python e pode ajudá-lo a se destacar na comunidade de programação.

## Como fazer isso?

Para ler argumentos da linha de comando em Python, você precisa importar o módulo `sys`. Em seguida, você pode acessar os argumentos fornecidos na linha de comando através da variável `argv`. O primeiro argumento sempre será o nome do seu código Python, seguido dos argumentos específicos que você deseja fornecer.

Aqui está um exemplo de como você pode ler dois argumentos da linha de comando e imprimir seu conteúdo:

```python
import sys

argumentos = sys.argv

print("Nome do script:", argumentos[0])
print("Primeiro argumento:", argumentos[1])
print("Segundo argumento:", argumentos[2])

```

Ao executar esse código através da linha de comando com os argumentos "python blog.py post1 post2", você terá a seguinte saída:

```
Nome do script: blog.py 
Primeiro argumento: post1 
Segundo argumento: post2
```

Você pode acessar os argumentos da linha de comando como strings e usar como entrada para suas funções e algoritmos. Isso permite que seus programas sejam mais personalizáveis e dinâmicos.

## Mergulho Profundo

Ao ler argumentos da linha de comando, é importante considerar algumas práticas recomendadas. Aqui estão algumas dicas para ajudá-lo a lidar com argumentos da linha de comando em seus projetos Python:

- Lembre-se de verificar a quantidade de argumentos fornecidos pelo usuário antes de tentar acessá-los. Você pode usar a função `len()` para verificar o comprimento da lista de argumentos.

- Se você quiser fornecer opções ao usuário (como "ajuda" ou "versão"), pode usar o módulo `argparse` em vez de acessar os argumentos diretamente através da variável `argv`.

- Seus argumentos podem conter espaços ou caracteres especiais que precisam ser tratados adequadamente. Você pode usar a biblioteca `shlex` para ajudá-lo a separar seus argumentos em uma lista segura.

## Veja também

- [Documentação oficial do Python sobre leitura de argumentos da linha de comando](https://docs.python.org/3/library/sys.html#sys.argv)
- [Guia de Prática Recomendada do Real Python sobre como lidar com argumentos da linha de comando](https://realpython.com/python-command-line-arguments/)
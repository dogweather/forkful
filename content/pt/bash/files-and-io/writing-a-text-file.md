---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:08.193191-07:00
description: "Escrever um arquivo de texto em Bash permite automatizar o armazenamento\
  \ de dados, registro, configura\xE7\xF5es e muito mais. \xC9 uma habilidade fundamental\
  \ para\u2026"
lastmod: 2024-02-19 22:05:05.826979
model: gpt-4-0125-preview
summary: "Escrever um arquivo de texto em Bash permite automatizar o armazenamento\
  \ de dados, registro, configura\xE7\xF5es e muito mais. \xC9 uma habilidade fundamental\
  \ para\u2026"
title: Escrevendo um arquivo de texto
---

{{< edit_this_page >}}

## O Que & Por Quê?

Escrever um arquivo de texto em Bash permite automatizar o armazenamento de dados, registro, configurações e muito mais. É uma habilidade fundamental para scripts de shell, possibilitando aos programadores salvar a saída de comandos, execuções de scripts ou entrada do usuário para relatórios, processamento ou execução futura.

## Como fazer:

O Bash oferece métodos simples para escrever em um arquivo. Os mais comuns são usar operadores de redirecionamento (`>`, `>>`) e o comando `tee`. Aqui está uma rápida olhada em ambas as técnicas.

Usando redirecionamento, você pode escrever a saída diretamente em um arquivo. O operador `>` escreve conteúdo em um arquivo, substituindo-o se já existir, enquanto `>>` acrescenta a um arquivo existente sem deletar seu conteúdo.

```bash
# Escrevendo em um arquivo com >
echo "Olá, Mundo!" > myfile.txt

# Acrescentando a um arquivo com >>
echo "Esta é uma nova linha." >> myfile.txt
```

Se você verificar o conteúdo de `myfile.txt` após executar os comandos acima, encontraria:

```
Olá, Mundo!
Esta é uma nova linha.
```

O comando `tee` é prático quando você deseja escrever em um arquivo e ver a saída na tela (stdout) simultaneamente. Por padrão, `tee` sobrescreve o arquivo, mas com a flag `-a`, ele acrescenta ao arquivo.

```bash
# Escrevendo e exibindo usando tee
echo "Olá, novamente!" | tee myfile.txt

# Acrescentando e exibindo usando tee -a
echo "Adicionando outra linha." | tee -a myfile.txt
```

Após executar estes, `myfile.txt` exibirá:

```
Olá, novamente!
Adicionando outra linha.
```

Embora o próprio Bash forneça capacidades robustas de manipulação de arquivos através do redirecionamento e comandos como `tee`, manipulações adicionais ou cenários mais complexos podem exigir a chamada de ferramentas externas ou linguagens de script (por exemplo, Awk, Sed, Python) que oferecem funções de processamento de texto mais sofisticadas. No entanto, para a maioria das tarefas de escrita de arquivos diretas, os métodos acima são totalmente suficientes e amplamente utilizados.

---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:01.475370-07:00
description: "Express\xF5es regulares (regex) no Bash permitem que voc\xEA pesquise,\
  \ manipule e manuseie strings e arquivos com base em padr\xF5es espec\xEDficos.\
  \ Programadores\u2026"
lastmod: '2024-03-13T22:44:46.740993-06:00'
model: gpt-4-0125-preview
summary: "Express\xF5es regulares (regex) no Bash permitem que voc\xEA pesquise, manipule\
  \ e manuseie strings e arquivos com base em padr\xF5es espec\xEDficos. Programadores\u2026"
title: "Usando express\xF5es regulares"
---

{{< edit_this_page >}}

## O Que & Por Que?

Expressões regulares (regex) no Bash permitem que você pesquise, manipule e manuseie strings e arquivos com base em padrões específicos. Programadores usam regex para tarefas como validação de entrada, análise de arquivos de log e extração de dados porque oferece uma maneira flexível e poderosa de especificar padrões para necessidades complexas de processamento de texto.

## Como:

### Correspondência de Padrões Básica
Para encontrar se uma string corresponde a um padrão, você pode usar `grep`, uma utilidade de linha de comando para pesquisar conjuntos de dados de texto simples para linhas que correspondam a uma expressão regular:

```bash
echo "Olá, Mundo!" | grep -o "Mundo"
# Saída: Mundo
```

### Extraindo Dados Específicos
Para extrair partes de dados que correspondam aos seus padrões de regex, você pode usar `-o` com `grep`:

```bash
echo "Erro: Arquivo não encontrado" | grep -oE "[A-Za-z]+:"
# Saída: Erro:
```

### Usando Regex com `sed`
`sed` (editor de fluxo) é uma utilidade poderosa para analisar e transformar texto. Veja como usar `sed` com regex para substituir texto:

```bash
echo "Bash é ótimo" | sed -e 's/ótimo/increível/'
# Saída: Bash é incrível
```

### Correspondência de Padrões em Declarações Condicionais
Bash também suporta regex diretamente em declarações condicionais:

```bash
[[ "https://exemplo.com" =~ ^https?:// ]] && echo "URL é válida" || echo "URL é inválida"
# Saída: URL é válida
```

### Correspondência de Padrões Avançada e Manipulação com `awk`
`awk` é outra ferramenta de processamento de texto que suporta extração e manipulação de dados mais complexos. Pode ser benéfico ao trabalhar com dados de texto estruturados, como CSVs:

```bash
echo -e "ID,Nome,Idade\n1,João,22\n2,Ana,24" | awk -F, '$3 > 22 {print $2 " é mais velho(a) que 22."}'
# Saída: Ana é mais velho(a) que 22.
```

Embora as funcionalidades incorporadas de regex do Bash cubram muitos casos de uso, para operações de regex muito avançadas, você pode considerar usar uma combinação de scripts Bash com scripts `perl` ou `python`, já que essas linguagens oferecem poderosas bibliotecas de regex (por exemplo, `re` em Python). Um exemplo simples com Python:

```bash
echo "Capture isso 123" | python3 -c "import sys; import re; print(re.search('(\d+)', sys.stdin.read()).group(0))"
# Saída: 123
```

Incorporar essas linguagens de programação quando necessário pode ajudá-lo a aproveitar o pleno poder do regex em seus scripts Bash.

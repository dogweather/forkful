---
date: 2024-01-20 17:42:22.759639-07:00
description: "Como fazer: Historicamente, a manipula\xE7\xE3o de strings \xE9 uma\
  \ opera\xE7\xE3o fundamental em programa\xE7\xE3o. Ferramentas como `sed`, `awk`\
  \ e `grep` no Unix s\xE3o\u2026"
lastmod: '2024-04-05T21:53:47.341043-06:00'
model: gpt-4-1106-preview
summary: "Historicamente, a manipula\xE7\xE3o de strings \xE9 uma opera\xE7\xE3o fundamental\
  \ em programa\xE7\xE3o."
title: "Excluindo caracteres que correspondem a um padr\xE3o"
weight: 5
---

## Como fazer:
```Fish Shell
# Exemplo: Removendo todos os números de uma string
set string "u5u4r1o!"
echo $string | string replace -ar '[0-9]' ''
# Saída: usuro!

# Exemplo: Deletando espaços em branco no início e no fim da string
set frase "   Olá, Mundo!   "
echo $frase | string trim
# Saída: Olá, Mundo!

# Exemplo: Removendo hífens de um CPF
set cpf "123.456.789-10"
echo $cpf | string replace -ar '[.-]' ''
# Saída: 12345678910
```

## Aprofundamento
Historicamente, a manipulação de strings é uma operação fundamental em programação. Ferramentas como `sed`, `awk` e `grep` no Unix são precursores nesse contexto e ainda são amplamente utilizadas. No Fish Shell, `string` é um comando embutido que oferece funcionalidades de manipulação de strings poderosas e de alto nível. Utilizar o comando `string` no Fish é uma forma mais amigável e legível do que suas contrapartes mais antigas que geralmente requerem expressões regulares complexas.

Alternativas ao `string` incluem o uso de outros comandos Unix mencionados ou linguagens de programação como Python e Perl, onde essas operações podem ser mais verbosas, mas também mais poderosas e flexíveis para scripts mais complexos.

Ao deletar caracteres que correspondem a um padrão, o Fish Shell usa sua sintaxe de expressão regular interna para identificar as peças da string a serem removidas. Importante mencionar que, dependendo da complexidade da manipulação que precisa ser feita, às vezes pode ser necessário encadear múltiplas operações do comando `string` para alcançar o resultado desejado.

## Veja também
- Documentação oficial do comando string no Fish: [fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Tutorial sobre expressões regulares: [regular-expressions.info](https://www.regular-expressions.info/)
- Uma explicação sobre manipulação de strings no contexto do Unix: [GNU sed manual](https://www.gnu.org/software/sed/manual/sed.html)

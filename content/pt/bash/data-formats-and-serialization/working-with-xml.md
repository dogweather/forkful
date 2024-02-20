---
date: 2024-01-26 04:27:56.491997-07:00
description: "Trabalhar com XML envolve o parseamento, extra\xE7\xE3o e manipula\xE7\
  \xE3o de dados no formato da Linguagem de Marca\xE7\xE3o Extens\xEDvel. Programadores\
  \ lidam com o XML j\xE1\u2026"
lastmod: 2024-02-19 22:05:05.833403
model: gpt-4-0125-preview
summary: "Trabalhar com XML envolve o parseamento, extra\xE7\xE3o e manipula\xE7\xE3\
  o de dados no formato da Linguagem de Marca\xE7\xE3o Extens\xEDvel. Programadores\
  \ lidam com o XML j\xE1\u2026"
title: Trabalhando com XML
---

{{< edit_this_page >}}

## O Que & Por Quê?
Trabalhar com XML envolve o parseamento, extração e manipulação de dados no formato da Linguagem de Marcação Extensível. Programadores lidam com o XML já que ele é um formato de troca de dados amplamente utilizado para configurações, APIs e mais.

## Como Fazer:
Aqui está como fazer o parse de XML em Bash. Ferramentas? xmllint e xmlstarlet. Percorrer os elementos XML? Com certeza. Exemplo com saída de amostra:

```bash
# Supondo que o xmlstarlet esteja instalado
# Instale com: apt-get install xmlstarlet

# Fazendo o parse do conteúdo XML
cat <<EOF > sample.xml
<frutas>
  <fruta nome="Maçã"/>
  <fruta nome="Banana"/>
</frutas>
EOF

# Extrair nomes com xmlstarlet
xmlstarlet sel -t -m "//fruta" -v "@nome" -n sample.xml

# A saída deve ser:
# Maçã
# Banana
```

## Mergulho Profundo
Nos anos 90, o XML surgiu como uma alternativa mais simples ao SGML, mas mais estruturada do que o HTML. Agora, ele tem companhia – JSON, YAML, por exemplo. Mas o XML ainda está na ativa, especialmente em configurações e serviços web baseados em SOAP.

Em termos de ferramentas, o xmllint é confortável para validação de XML, consultas xpath. O xmlstarlet é o canivete suíço para truques com XML – consultar, editar, validar, transformar. Em scripts bash, eles são super-heróis para tarefas XML.

Sob o capô, o xmllint usa o libxml2 – o parser C para XML. É rápido, mas as mensagens de erro? Crípticas. E o xmlstarlet? Modelos recursivos e o suporte ao EXSLT. Difícil de entender, mas poderoso.

## Veja Também
- [xmlsoft.org](http://xmlsoft.org/): Coisas sobre Libxml2 e xmllint.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/xml+bash): Problemas e soluções do mundo real.
- [Tutorial XML do W3Schools](https://www.w3schools.com/xml/): Básicos do XML.

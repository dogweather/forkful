---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:44.658219-07:00
description: "Arrays associativos, conhecidos em outras linguagens como mapas ou dicion\xE1\
  rios, s\xE3o pares de chave-valor usados para a busca e manipula\xE7\xE3o eficiente\
  \ de\u2026"
lastmod: '2024-03-13T22:44:47.041942-06:00'
model: gpt-4-0125-preview
summary: "Arrays associativos, conhecidos em outras linguagens como mapas ou dicion\xE1\
  rios, s\xE3o pares de chave-valor usados para a busca e manipula\xE7\xE3o eficiente\
  \ de dados."
title: Utilizando arrays associativos
weight: 15
---

## Como fazer:
C não possui suporte embutido para arrays associativos como algumas linguagens de mais alto nível, mas você pode simulá-los usando estruturas e hashing. Abaixo está um exemplo simplista usando uma combinação de uma struct e uma função de hashing simples para implementar um array associativo para armazenar e acessar inteiros por chaves de string.

Primeiro, defina uma estrutura para representar um único par chave-valor e outra para representar o próprio array associativo:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TABLE_SIZE 128

typedef struct {
    char* chave;
    int valor;
} ParChaveValor;

typedef struct {
    ParChaveValor* itens[TABLE_SIZE];
} ArrayAssoc;

unsigned int hash(char* chave) {
    unsigned long int valor = 0;
    unsigned int i = 0;
    unsigned int comprimento_chave = strlen(chave);

    for (; i < comprimento_chave; ++i) {
        valor = valor * 37 + chave[i];
    }

    valor = valor % TABLE_SIZE;

    return valor;
}

void initArray(ArrayAssoc* array) {
    for (int i = 0; i < TABLE_SIZE; ++i) {
        array->itens[i] = NULL;
    }
}

void inserir(ArrayAssoc* array, char* chave, int valor) {
    unsigned int slot = hash(chave);

    ParChaveValor* item = (ParChaveValor*)malloc(sizeof(ParChaveValor));
    item->chave = strdup(chave);
    item->valor = valor;

    array->itens[slot] = item;
}

int encontrar(ArrayAssoc* array, char* chave) {
    unsigned int slot = hash(chave);

    if (array->itens[slot]) {
        return array->itens[slot]->valor;
    }
    return -1;
}

int main() {
    ArrayAssoc a;
    initArray(&a);

    inserir(&a, "chave1", 1);
    inserir(&a, "chave2", 2);

    printf("%d\n", encontrar(&a, "chave1")); // Saída: 1
    printf("%d\n", encontrar(&a, "chave2")); // Saída: 2

    return 0;
}
```

Este exemplo demonstra operações básicas: inicializando um array associativo, inserindo pares chave-valor e encontrando valores pelas chaves. Note que este código não trata colisões e é destinado a fins educacionais.

## Aprofundando
O conceito de arrays associativos antecede a linguagem C, mas a natureza de baixo nível da linguagem não os suporta diretamente como tipos embutidos. Isso encoraja um entendimento mais profundo das estruturas de dados e algoritmos, incluindo mecanismos de hashing para mapeamento eficiente de chave-valor. Muitas bibliotecas e frameworks de C oferecem abordagens mais sofisticadas para implementar arrays associativos, como o `GHashTable` da GLib, que fornece uma implementação robusta completada com tratamento de colisões, redimensionamento dinâmico e suporte para tipos de chaves e valores arbitrários.

Embora a construção manual de arrays associativos em C possa ser vista como trabalhosa comparada a linguagens com suporte embutido, ela oferece insights inestimáveis sobre o funcionamento interno das estruturas de dados, aprimorando as habilidades de resolução de problemas e otimização dos programadores. No entanto, para códigos de produção ou aplicações mais complexas, aproveitar bibliotecas existentes como a GLib geralmente é uma abordagem mais prática e eficiente em termos de tempo.

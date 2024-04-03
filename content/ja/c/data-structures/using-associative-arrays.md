---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:49.497799-07:00
description: "\u9023\u60F3\u914D\u5217\u306F\u3001\u4ED6\u306E\u8A00\u8A9E\u3067\u306F\
  \u30DE\u30C3\u30D7\u3084\u8F9E\u66F8\u3068\u3057\u3066\u77E5\u3089\u308C\u3066\u304A\
  \u308A\u3001\u52B9\u7387\u7684\u306A\u30C7\u30FC\u30BF\u306E\u691C\u7D22\u3068\u64CD\
  \u4F5C\u306B\u4F7F\u7528\u3055\u308C\u308B\u30AD\u30FC\u3068\u5024\u306E\u30DA\u30A2\
  \u3067\u3059\u3002\u5F93\u6765\u306E\u914D\u5217\u304C\u6574\u6570\u30A4\u30F3\u30C7\
  \u30C3\u30AF\u30B9\u3092\u4F7F\u7528\u3059\u308B\u306E\u306B\u5BFE\u3057\u3001\u9023\
  \u60F3\u914D\u5217\u3067\u306F\u30AD\u30FC\u3092\u4F7F\u7528\u3059\u308B\u305F\u3081\
  \u3001\u30C7\u30FC\u30BF\u3078\u306E\u30A2\u30AF\u30BB\u30B9\u304C\u3088\u308A\u76F4\
  \u611F\u7684\u3067\u67D4\u8EDF\u306B\u306A\u308A\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306B\u3068\u3063\u3066\u4FBF\u5229\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:42.786021-06:00'
model: gpt-4-0125-preview
summary: "\u9023\u60F3\u914D\u5217\u306F\u3001\u4ED6\u306E\u8A00\u8A9E\u3067\u306F\
  \u30DE\u30C3\u30D7\u3084\u8F9E\u66F8\u3068\u3057\u3066\u77E5\u3089\u308C\u3066\u304A\
  \u308A\u3001\u52B9\u7387\u7684\u306A\u30C7\u30FC\u30BF\u306E\u691C\u7D22\u3068\u64CD\
  \u4F5C\u306B\u4F7F\u7528\u3055\u308C\u308B\u30AD\u30FC\u3068\u5024\u306E\u30DA\u30A2\
  \u3067\u3059\u3002\u5F93\u6765\u306E\u914D\u5217\u304C\u6574\u6570\u30A4\u30F3\u30C7\
  \u30C3\u30AF\u30B9\u3092\u4F7F\u7528\u3059\u308B\u306E\u306B\u5BFE\u3057\u3001\u9023\
  \u60F3\u914D\u5217\u3067\u306F\u30AD\u30FC\u3092\u4F7F\u7528\u3059\u308B\u305F\u3081\
  \u3001\u30C7\u30FC\u30BF\u3078\u306E\u30A2\u30AF\u30BB\u30B9\u304C\u3088\u308A\u76F4\
  \u611F\u7684\u3067\u67D4\u8EDF\u306B\u306A\u308A\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306B\u3068\u3063\u3066\u4FBF\u5229\u3067\u3059\u3002."
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
weight: 15
---

## 何となぜ？

連想配列は、他の言語ではマップや辞書として知られており、効率的なデータの検索と操作に使用されるキーと値のペアです。従来の配列が整数インデックスを使用するのに対し、連想配列ではキーを使用するため、データへのアクセスがより直感的で柔軟になり、プログラマーにとって便利です。

## どのようにして：

C言語には、いくつかの高レベル言語のような連想配列の組み込みサポートはありませんが、構造体とハッシュを使用してそれをシミュレートすることができます。以下は、文字列キーによって整数を格納およびアクセスするための連想配列を実装するために、構造体と単純なハッシュ関数を組み合わせた簡素な例です。

まず、単一のキーと値のペアを表す構造体と、連想配列自体を表す別の構造体を定義します:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TABLE_SIZE 128

typedef struct {
    char* key;
    int value;
} KeyValuePair;

typedef struct {
    KeyValuePair* items[TABLE_SIZE];
} AssocArray;

unsigned int hash(char* key) {
    unsigned long int value = 0;
    unsigned int i = 0;
    unsigned int key_len = strlen(key);

    for (; i < key_len; ++i) {
        value = value * 37 + key[i];
    }

    value = value % TABLE_SIZE;

    return value;
}

void initArray(AssocArray* array) {
    for (int i = 0; i < TABLE_SIZE; ++i) {
        array->items[i] = NULL;
    }
}

void insert(AssocArray* array, char* key, int value) {
    unsigned int slot = hash(key);

    KeyValuePair* item = (KeyValuePair*)malloc(sizeof(KeyValuePair));
    item->key = strdup(key);
    item->value = value;

    array->items[slot] = item;
}

int find(AssocArray* array, char* key) {
    unsigned int slot = hash(key);

    if (array->items[slot]) {
        return array->items[slot]->value;
    }
    return -1;
}

int main() {
    AssocArray a;
    initArray(&a);

    insert(&a, "key1", 1);
    insert(&a, "key2", 2);

    printf("%d\n", find(&a, "key1")); // 出力: 1
    printf("%d\n", find(&a, "key2")); // 出力: 2

    return 0;
}
```

この例は、連想配列の初期化、キーと値のペアの挿入、そしてキーによる値の検索といった基本的な操作を示しています。このコードには衝突処理が含まれておらず、教育目的での使用を意図しています。

## 深掘り

連想配列の概念はC言語よりも前に存在していましたが、C言語の低水準な性質はそれらを組み込み型として直接サポートしていません。これは、効率的なキーと値のマッピングのためのハッシュメカニズムなど、データ構造やアルゴリズムについての深い理解を促進します。多くのC言語のライブラリやフレームワークは、衝突処理、動的なリサイズ、任意のキーと値の型のサポートを備えた堅牢な実装を提供するGLibの`GHashTable`のように、連想配列を実装するためのより洗練されたアプローチを提供しています。

C言語で連想配列を手動で構築することは、組み込みサポートのある言語と比較して手間がかかると見なされる場合がありますが、データ構造の内部動作について貴重な洞察を提供し、問題解決や最適化においてプログラマーのスキルを磨くために非常に価値があります。しかし、本番コードやより複雑なアプリケーションの場合、GLibのような既存のライブラリを活用することが、より実用的で時間効率の良いアプローチであることが多いです。

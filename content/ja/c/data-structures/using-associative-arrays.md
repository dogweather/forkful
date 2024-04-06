---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:49.497799-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A C\u8A00\u8A9E\u306B\
  \u306F\u3001\u3044\u304F\u3064\u304B\u306E\u9AD8\u30EC\u30D9\u30EB\u8A00\u8A9E\u306E\
  \u3088\u3046\u306A\u9023\u60F3\u914D\u5217\u306E\u7D44\u307F\u8FBC\u307F\u30B5\u30DD\
  \u30FC\u30C8\u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001\u69CB\u9020\u4F53\u3068\
  \u30CF\u30C3\u30B7\u30E5\u3092\u4F7F\u7528\u3057\u3066\u305D\u308C\u3092\u30B7\u30DF\
  \u30E5\u30EC\u30FC\u30C8\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\
  \u4EE5\u4E0B\u306F\u3001\u6587\u5B57\u5217\u30AD\u30FC\u306B\u3088\u3063\u3066\u6574\
  \u6570\u3092\u683C\u7D0D\u304A\u3088\u3073\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u305F\
  \u3081\u306E\u9023\u60F3\u914D\u5217\u3092\u5B9F\u88C5\u3059\u308B\u305F\u3081\u306B\
  \u3001\u69CB\u9020\u4F53\u3068\u5358\u7D14\u306A\u30CF\u30C3\u30B7\u30E5\u95A2\u6570\
  \u3092\u7D44\u307F\u5408\u308F\u305B\u305F\u7C21\u7D20\u306A\u4F8B\u3067\u3059\u3002\
  \u2026"
lastmod: '2024-04-05T21:53:43.568837-06:00'
model: gpt-4-0125-preview
summary: "\u307E\u305A\u3001\u5358\u4E00\u306E\u30AD\u30FC\u3068\u5024\u306E\u30DA\
  \u30A2\u3092\u8868\u3059\u69CB\u9020\u4F53\u3068\u3001\u9023\u60F3\u914D\u5217\u81EA\
  \u4F53\u3092\u8868\u3059\u5225\u306E\u69CB\u9020\u4F53\u3092\u5B9A\u7FA9\u3057\u307E\
  \u3059."
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
weight: 15
---

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

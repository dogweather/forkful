---
title:                "JSONを利用する"
aliases:
- /ja/c/working-with-json/
date:                  2024-02-03T18:12:32.269210-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSONを利用する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となく理由

C言語でJSON（JavaScript Object Notation）を扱うことは、JSONデータ構造を解析、生成、そして操作することを意味します。プログラマーは、軽量で人間が読みやすいフォーマットでのwebサービスとの通信、データ保管、または設定ファイルなどのためにこれを行います。

## 方法

C言語でJSONを扱うためには、C言語にはJSONの組み込みサポートがないため、通常は`jansson`や`json-c`のようなライブラリを使うことになります。ここでは、使いやすさと活発なメンテナンスのために、`jansson`に焦点を当てます。まず、ライブラリをインストールします（例えば、Ubuntuの`apt`のようなパッケージマネージャーを使って：`sudo apt-get install libjansson-dev`）。

JSON文字列を解析し、その内容にアクセスすることから始めましょう：

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    const char *json_string = "{\"name\":\"John Doe\",\"age\":30}";
    json_error_t error;
    json_t *root = json_loads(json_string, 0, &error);
    
    if(!root) {
        fprintf(stderr, "error: on line %d: %s\n", error.line, error.text);
        return 1;
    }
    
    const char *name;
    int age;
    json_unpack(root, "{s:s, s:i}", "name", &name, "age", &age);
    
    printf("名前: %s\n年齢: %d\n", name, age);
    
    json_decref(root);
    return 0;
}
```

サンプル出力：
```
名前: John Doe
年齢: 30
```

次に、JSONオブジェクトを作成して出力する例：

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    json_t *root = json_object();
    json_object_set_new(root, "name", json_string("Jane Doe"));
    json_object_set_new(root, "age", json_integer(25));
    
    char *json_dump = json_dumps(root, JSON_ENCODE_ANY);
    printf("%s\n", json_dump);
    
    free(json_dump);
    json_decref(root);
    return 0;
}
```

サンプル出力：
```
{"name": "Jane Doe", "age": 25}
```

これらの例は、JSON文字列を読み込み、その値をアンパックし、新しいJSONオブジェクトを作成して、それを文字列として出力する基本を示しています。

## 深い掘り下げ

C言語でJSONを扱う必要性は、webがデータ交換の主要なフォーマットとしてJSONを採用したことから発生します。JSONの単純さと効率性が、C言語がJSON操作の直接的なサポートを欠いていたにも関わらず、迅速にXMLを凌駕しました。初期の解決策には手動での文字列操作が含まれていました -これはエラーが発生しやすく非効率的でした。`jansson`や`json-c` のようなライブラリがこのギャップを埋めるために登場し、JSONの解析、構築、およびシリアライズのための堅牢なAPIを提供しました。

`jansson`は使いやすさと簡便性を提供する一方で、より広範な機能セットを求める人には`json-c`が魅力的かもしれません。それにもかかわらず、C++のような解析ライブラリは、より複雑なデータ構造と標準ライブラリサポートのおかげで、より洗練された抽象化を提供します。しかし、埋め込みシステムや既存のCライブラリとのインターフェースとしてCが好まれたり必要とされたりする環境で作業する場合、`jansson`や`json-c`の使用は不可欠となります。

また、C言語でJSONを扱うことは、これらのライブラリが頻繁に動的に割り当てられたオブジェクトを返すため、明示的な解放が必要なメモリ管理の深い理解を含んでいることにも注意する価値があります。これは、プログラマーにとって、便利さとメモリリークを防ぐ責任とのバランスをとる挑戦であり、効率的なCコードを作成する上で重要な側面です。

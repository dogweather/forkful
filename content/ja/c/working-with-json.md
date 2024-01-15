---
title:                "「JSONを扱う」"
html_title:           "C: 「JSONを扱う」"
simple_title:         "「JSONを扱う」"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/working-with-json.md"
---

{{< edit_this_page >}}

## なぜ

JSONを使えば、簡単にデータをストアや交換することができます。C言語を使ってJSONを使う方法を学ぶことは、より効率的なプログラミングに役立ちます。

## 方法

[JSON-C](https://github.com/json-c/json-c)ライブラリを使えば、C言語で簡単にJSONを扱うことができます。まずはライブラリをダウンロードし、プロジェクトに追加しましょう。

```C
#include <json-c/json.h>
```
ライブラリをインクルードした後、JSONデータを作成する方法を学びましょう。以下の例では、オブジェクトを作成し、値を追加しています。

```C
json_object *my_json = json_object_new_object(); // オブジェクトを作成
json_object *name = json_object_new_string("John"); // 値を追加
json_object_object_add(my_json, "name", name); // オブジェクトに追加
```
JSONデータを表示するには、`json_object_to_json_string()`関数を使用します。

```C
printf("%s\n", json_object_to_json_string(my_json)); // {"name": "John"}
```

## ディープダイブ

JSON-Cライブラリにはさまざまな機能がありますが、その中でも特に重要なものをいくつか紹介しましょう。

1. `json_object_object_get()`関数を使えば、オブジェクトから値を取り出すことができます。
2. `json_object_array_length()`関数を使えば、配列の長さを取得できます。
3. `json_object_object_foreach()`関数を使うと、オブジェクト内のキーと値を繰り返し処理することができます。

詳細な使い方や他の機能については、[公式ドキュメント](https://github.com/json-c/json-c/wiki/JSON-C-Examples)を参考にしてください。

## 関連リンク

[JSON-Cライブラリ公式リポジトリ](https://github.com/json-c/json-c)、[JSON-Cドキュメント](https://github.com/json-c/json-c/wiki)
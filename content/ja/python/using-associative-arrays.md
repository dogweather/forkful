---
title:                "連想配列の使用"
aliases:
- ja/python/using-associative-arrays.md
date:                  2024-01-30T19:12:55.531588-07:00
model:                 gpt-4-0125-preview
simple_title:         "連想配列の使用"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

連想配列はPythonでは辞書として知られており、キーを値にマッピングし、ユニークな識別子でデータを簡単に取得、変更、または追跡できるようにします。プログラマーは、要素へのアクセス効率と複雑なデータ構造を表現する柔軟性のためにそれらを使用しています。

## どのようにして：

Pythonで辞書を作成するのは簡単です。キーと値を中括弧`{}`で括り、キーと値をコロンで区切ります：

```Python
# 連想配列（辞書）を作成する
my_dict = {"name": "John", "age": 30, "city": "New York"}
print(my_dict)
```

出力：
```
{'name': 'John', 'age': 30, 'city': 'New York'}
```

キーによって値にアクセスするのは簡単です：

```Python
# 値にアクセスする
print(my_dict["name"])
```

出力：
```
John
```

要素を追加または更新するには、キーに値を割り当てます：

```Python
# 新しいキーと値のペアを追加する
my_dict["email"] = "john@example.com"
# 値を更新する
my_dict["age"] = 31
print(my_dict)
```

出力：
```
{'name': 'John', 'age': 31, 'city': 'New York', 'email': 'john@example.com'}
```

辞書の項目を反復処理するには：

```Python
# キーと値のペアを反復処理する
for key, value in my_dict.items():
    print(f"{key}: {value}")
```

出力：
```
name: John
age: 31
city: New York
email: john@example.com
```

## ディープダイブ

Pythonの連想配列、または辞書は、効率的なデータアクセスと操作のためのデータ構造を提供するために導入されました。シーケンスが一連の数字によって索引付けられるのとは異なり、辞書は任意の不変タイプのキーによって索引付けられます。この設計選択により、辞書はキーがユニークな値にマップする高速検索テーブルに最適です。

歴史的に、Pythonの辞書はハッシュテーブルを使用して実装されており、検索、挿入、削除操作の平均時間計算量がO(1)であることを保証しています。Python 3.6以降では、辞書はアイテムの挿入順序を保持するようになり、ハッシュテーブルの利点と順序付きデータ構造に見られる挿入順序の予測可能性を組み合わせています。

辞書は非常に多用途性がありますが、特定の専門的な場合には、`collections.defaultdict`やPython 3.7以前の`collections.OrderedDict`などの代替品が好ましい場合があります。`defaultdict`は、存在しないキーのデフォルト値を辞書に返す必要がある場合に特に便利で、特定の条件付きロジックを簡素化します。しかし、Pythonの継続的な改善と進化により、ビルトインの辞書クラスはしばしばその堅牢性と箱から出してすぐに利用できる利便性のために、連想配列のための主要な選択肢として残ります。

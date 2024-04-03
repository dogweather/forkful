---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:55.531588-07:00
description: "\u9023\u60F3\u914D\u5217\u306FPython\u3067\u306F\u8F9E\u66F8\u3068\u3057\
  \u3066\u77E5\u3089\u308C\u3066\u304A\u308A\u3001\u30AD\u30FC\u3092\u5024\u306B\u30DE\
  \u30C3\u30D4\u30F3\u30B0\u3057\u3001\u30E6\u30CB\u30FC\u30AF\u306A\u8B58\u5225\u5B50\
  \u3067\u30C7\u30FC\u30BF\u3092\u7C21\u5358\u306B\u53D6\u5F97\u3001\u5909\u66F4\u3001\
  \u307E\u305F\u306F\u8FFD\u8DE1\u3067\u304D\u308B\u3088\u3046\u306B\u3057\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u8981\u7D20\u3078\u306E\u30A2\
  \u30AF\u30BB\u30B9\u52B9\u7387\u3068\u8907\u96D1\u306A\u30C7\u30FC\u30BF\u69CB\u9020\
  \u3092\u8868\u73FE\u3059\u308B\u67D4\u8EDF\u6027\u306E\u305F\u3081\u306B\u305D\u308C\
  \u3089\u3092\u4F7F\u7528\u3057\u3066\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.490502-06:00'
model: gpt-4-0125-preview
summary: "\u9023\u60F3\u914D\u5217\u306FPython\u3067\u306F\u8F9E\u66F8\u3068\u3057\
  \u3066\u77E5\u3089\u308C\u3066\u304A\u308A\u3001\u30AD\u30FC\u3092\u5024\u306B\u30DE\
  \u30C3\u30D4\u30F3\u30B0\u3057\u3001\u30E6\u30CB\u30FC\u30AF\u306A\u8B58\u5225\u5B50\
  \u3067\u30C7\u30FC\u30BF\u3092\u7C21\u5358\u306B\u53D6\u5F97\u3001\u5909\u66F4\u3001\
  \u307E\u305F\u306F\u8FFD\u8DE1\u3067\u304D\u308B\u3088\u3046\u306B\u3057\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u8981\u7D20\u3078\u306E\u30A2\
  \u30AF\u30BB\u30B9\u52B9\u7387\u3068\u8907\u96D1\u306A\u30C7\u30FC\u30BF\u69CB\u9020\
  \u3092\u8868\u73FE\u3059\u308B\u67D4\u8EDF\u6027\u306E\u305F\u3081\u306B\u305D\u308C\
  \u3089\u3092\u4F7F\u7528\u3057\u3066\u3044\u307E\u3059\u3002."
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
weight: 15
---

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

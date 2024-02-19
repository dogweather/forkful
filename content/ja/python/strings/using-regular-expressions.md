---
aliases:
- /ja/python/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:17.735890-07:00
description: "\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\u306F\u3001\u6587\u5B57\u5217\
  \u5185\u306E\u6587\u5B57\u306E\u7D44\u307F\u5408\u308F\u305B\u306B\u4E00\u81F4\u3059\
  \u308B\u305F\u3081\u306B\u4F7F\u7528\u3055\u308C\u308B\u30D1\u30BF\u30FC\u30F3\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u5B9A\u7FA9\u3055\u308C\
  \u305F\u30D1\u30BF\u30FC\u30F3\u306B\u57FA\u3065\u3044\u3066\u30C6\u30AD\u30B9\u30C8\
  \u3092\u691C\u7D22\u3001\u7DE8\u96C6\u3001\u307E\u305F\u306F\u64CD\u4F5C\u3059\u308B\
  \u305F\u3081\u306B\u3053\u308C\u3092\u5229\u7528\u3057\u3001\u30C7\u30FC\u30BF\u691C\
  \u8A3C\u3001\u30D1\u30FC\u30B9\u307E\u305F\u306F\u5909\u63DB\u306E\u3088\u3046\u306A\
  \u30BF\u30B9\u30AF\u306B\u4E0D\u53EF\u6B20\u3067\u3059\u3002"
lastmod: 2024-02-18 23:08:54.554496
model: gpt-4-0125-preview
summary: "\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\u306F\u3001\u6587\u5B57\u5217\u5185\
  \u306E\u6587\u5B57\u306E\u7D44\u307F\u5408\u308F\u305B\u306B\u4E00\u81F4\u3059\u308B\
  \u305F\u3081\u306B\u4F7F\u7528\u3055\u308C\u308B\u30D1\u30BF\u30FC\u30F3\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u5B9A\u7FA9\u3055\u308C\u305F\
  \u30D1\u30BF\u30FC\u30F3\u306B\u57FA\u3065\u3044\u3066\u30C6\u30AD\u30B9\u30C8\u3092\
  \u691C\u7D22\u3001\u7DE8\u96C6\u3001\u307E\u305F\u306F\u64CD\u4F5C\u3059\u308B\u305F\
  \u3081\u306B\u3053\u308C\u3092\u5229\u7528\u3057\u3001\u30C7\u30FC\u30BF\u691C\u8A3C\
  \u3001\u30D1\u30FC\u30B9\u307E\u305F\u306F\u5909\u63DB\u306E\u3088\u3046\u306A\u30BF\
  \u30B9\u30AF\u306B\u4E0D\u53EF\u6B20\u3067\u3059\u3002"
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となぜ？
正規表現（regex）は、文字列内の文字の組み合わせに一致するために使用されるパターンです。プログラマーは、定義されたパターンに基づいてテキストを検索、編集、または操作するためにこれを利用し、データ検証、パースまたは変換のようなタスクに不可欠です。

## 使い方：
Pythonでregexを使用するには、正規表現を使用してテキストを処理するための一連の関数を提供する`re`モジュールを使用します。

### 基本的なパターンマッチング
文字列内でパターンを検索するには、`re.search()`を使用します。パターンが見つかると、マッチオブジェクトを返し、そうでなければ`None`を返します。
```python
import re

text = "Learn Python programming"
match = re.search("Python", text)
if match:
    print("パターンが見つかりました！")
else:
    print("パターンが見つかりませんでした。")
```
出力：
```
パターンが見つかりました！
```

### 正規表現のコンパイル
同じパターンを繰り返し使用する場合は、`re.compile()`で先にコンパイルして、パフォーマンスを向上させます。
```python
pattern = re.compile("Python")
match = pattern.search("Learn Python programming")
if match:
    print("コンパイルされたパターンが見つかりました！")
```
出力：
```
コンパイルされたパターンが見つかりました！
```

### 文字列の分割
正規表現パターンの各マッチで文字列を分割するには、`re.split()`を使用します。
```python
result = re.split("\s", "Python is fun")
print(result)
```
出力：
```
['Python', 'is', 'fun']
```

### すべてのマッチを探す
パターンの重なり合わない全ての発生を見つけるには、`re.findall()`を使用します。
```python
matches = re.findall("n", "Python programming")
print(matches)
```
出力：
```
['n', 'n']
```

### テキストの置換
`re.sub()`を使用して、パターンの発生を新しい文字列で置き換えます。
```python
replaced_text = re.sub("fun", "awesome", "Python is fun")
print(replaced_text)
```
出力：
```
Python is awesome
```

### サードパーティライブラリ
Pythonの組み込み`re`モジュールはパワフルですが、`regex`のようなサードパーティライブラリはより多くの機能と強化されたパフォーマンスを提供します。 `regex`を使用するには、pip（`pip install regex`）経由でインストールし、コードでインポートします。

```python
import regex

text = "Learning Python 3.8"
match = regex.search(r"Python\s(\d+\.\d+)", text)
if match:
    print(f"バージョンが見つかりました: {match.group(1)}")
```
出力：
```
バージョンが見つかりました: 3.8
```

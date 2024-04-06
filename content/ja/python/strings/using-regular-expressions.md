---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:17.735890-07:00
description: "\u4F7F\u3044\u65B9\uFF1A Python\u3067regex\u3092\u4F7F\u7528\u3059\u308B\
  \u306B\u306F\u3001\u6B63\u898F\u8868\u73FE\u3092\u4F7F\u7528\u3057\u3066\u30C6\u30AD\
  \u30B9\u30C8\u3092\u51E6\u7406\u3059\u308B\u305F\u3081\u306E\u4E00\u9023\u306E\u95A2\
  \u6570\u3092\u63D0\u4F9B\u3059\u308B`re`\u30E2\u30B8\u30E5\u30FC\u30EB\u3092\u4F7F\
  \u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:42.441586-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

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

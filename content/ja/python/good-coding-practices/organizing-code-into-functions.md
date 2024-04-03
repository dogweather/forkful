---
date: 2024-01-26 01:16:25.459420-07:00
description: "\u65B9\u6CD5: \u4F8B\u3048\u3070\u3001\u3042\u308B\u6570\u306E\u5E73\
  \u65B9\u3068\u7ACB\u65B9\u3092\u8A08\u7B97\u3059\u308B\u30B9\u30AF\u30EA\u30D7\u30C8\
  \u3092\u66F8\u3044\u3066\u3044\u308B\u3068\u3057\u307E\u3057\u3087\u3046\u3002\u95A2\
  \u6570\u3092\u4F7F\u308F\u306A\u3044\u3068\u3001\u7E70\u308A\u8FD4\u3057\u306E\u6DF7\
  \u4E71\u306B\u9665\u308A\u307E\u3059."
lastmod: '2024-03-13T22:44:41.506930-06:00'
model: gpt-4-0125-preview
summary: "\u4F8B\u3048\u3070\u3001\u3042\u308B\u6570\u306E\u5E73\u65B9\u3068\u7ACB\
  \u65B9\u3092\u8A08\u7B97\u3059\u308B\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u66F8\u3044\
  \u3066\u3044\u308B\u3068\u3057\u307E\u3057\u3087\u3046\u3002\u95A2\u6570\u3092\u4F7F\
  \u308F\u306A\u3044\u3068\u3001\u7E70\u308A\u8FD4\u3057\u306E\u6DF7\u4E71\u306B\u9665\
  \u308A\u307E\u3059."
title: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u6574\u7406\u3059\u308B"
weight: 18
---

## 方法:
例えば、ある数の平方と立方を計算するスクリプトを書いているとしましょう。関数を使わないと、繰り返しの混乱に陥ります:

```Python
num = 4
square = num * num
cube = num * num * num
print(f"平方: {square}, 立方: {cube}")

num = 5
square = num * num
cube = num * num * num
print(f"平方: {square}, 立方: {cube}")
```
出力:
```
平方: 16, 立方: 64
平方: 25, 立方: 125
```

関数を使って整理すると、見た目がすっきりします:

```Python
def square(n):
    return n * n

def cube(n):
    return n ** 3

num = 4
print(f"平方: {square(num)}, 立方: {cube(num)}")

num = 5
print(f"平方: {square(num)}, 立方: {cube(num)}")
```
出力:
```
平方: 16, 立方: 64
平方: 25, 立方: 125
```

## 深掘り
昔のように、プログラムがシンプルだった時代には、指示リストを書くだけで済んだこともありました。しかし、ソフトウェアが複雑になるにつれて、開発者は同じコードを何度も何度も書き直していることに気付きました。ここで関数の出番です—単一のアクションを実行する再利用可能なコードブロックです。

関数の代替手段にはクラス（操作するデータと関数をまとめること）やインラインコード（必要なところで直接知識を、しかし複雑なタスクにはリスキー）があります。実装上のコツは、関数を作るだけでなく、それが一つのことをうまく行うようにすることです—単一責任の原則を思い出してください。理想的には、関数はステートレスであるべきです、つまり、入ってくるデータや出ていくデータに驚きがないことを意味します。

## 参照
- 公式Pythonチュートリアルの関数について: https://docs.python.org/3/tutorial/controlflow.html#defining-functions
- 'Clean Code' by Robert C. Martin, クリーンな関数の書き方に関する原則が述べられています。
- 'Refactoring: Improving the Design of Existing Code' by Martin Fowler, コードの整理の例が含まれています。

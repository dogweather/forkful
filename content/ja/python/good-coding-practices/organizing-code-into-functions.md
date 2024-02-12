---
title:                "コードを関数に整理する"
aliases: - /ja/python/organizing-code-into-functions.md
date:                  2024-01-26T01:16:25.459420-07:00
model:                 gpt-4-0125-preview
simple_title:         "コードを関数に整理する"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## 何となぜ?
コードを関数にまとめるとは、コードを特定の目的を持った再利用可能なチャンクに分解することを指します。それを行う理由は、コードをよりクリーンにし、読みやすく、デバッグや更新を容易にするためです。

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

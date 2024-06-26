---
date: 2024-01-27 20:33:38.249618-07:00
description: "\u65B9\u6CD5\uFF1A Fish\u3067\u30E9\u30F3\u30C0\u30E0\u306A\u6570\u5024\
  \u3092\u751F\u6210\u3059\u308B\u3053\u3068\u306F\u3001\u30B7\u30B9\u30C6\u30E0\u30E6\
  \u30FC\u30C6\u30A3\u30EA\u30C6\u30A3\u3068\u30B7\u30A7\u30EB\u306E\u6A5F\u80FD\u306E\
  \u7D44\u307F\u5408\u308F\u305B\u3092\u4F7F\u7528\u3057\u3066\u3001\u7C21\u5358\u306B\
  \u884C\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306B\u3001\
  \u6307\u5B9A\u3055\u308C\u305F\u7BC4\u56F2\u5185\u3067\u30E9\u30F3\u30C0\u30E0\u306A\
  \u6570\u5024\u3092\u751F\u6210\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3059\u4F8B\u3092\
  \u3044\u304F\u3064\u304B\u7D39\u4ECB\u3057\u307E\u3059\u3002 **0\u3068100\u306E\u9593\
  \u3067\u30E9\u30F3\u30C0\u30E0\u306A\u6570\u5024\u3092\u751F\u6210\u3059\u308B\uFF1A\
  **."
lastmod: '2024-04-05T22:38:42.214680-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Fish\u3067\u30E9\u30F3\u30C0\u30E0\u306A\u6570\u5024\u3092\
  \u751F\u6210\u3059\u308B\u3053\u3068\u306F\u3001\u30B7\u30B9\u30C6\u30E0\u30E6\u30FC\
  \u30C6\u30A3\u30EA\u30C6\u30A3\u3068\u30B7\u30A7\u30EB\u306E\u6A5F\u80FD\u306E\u7D44\
  \u307F\u5408\u308F\u305B\u3092\u4F7F\u7528\u3057\u3066\u3001\u7C21\u5358\u306B\u884C\
  \u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306B\u3001\u6307\
  \u5B9A\u3055\u308C\u305F\u7BC4\u56F2\u5185\u3067\u30E9\u30F3\u30C0\u30E0\u306A\u6570\
  \u5024\u3092\u751F\u6210\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3059\u4F8B\u3092\u3044\
  \u304F\u3064\u304B\u7D39\u4ECB\u3057\u307E\u3059\u3002 **0\u3068100\u306E\u9593\u3067\
  \u30E9\u30F3\u30C0\u30E0\u306A\u6570\u5024\u3092\u751F\u6210\u3059\u308B\uFF1A**."
title: "\u4E71\u6570\u306E\u751F\u6210"
weight: 12
---

## 方法：
Fishでランダムな数値を生成することは、システムユーティリティとシェルの機能の組み合わせを使用して、簡単に行うことができます。以下に、指定された範囲内でランダムな数値を生成する方法を示す例をいくつか紹介します。

**0と100の間でランダムな数値を生成する：**

```fish
set -l rand_num (random 0 100)
echo $rand_num
```

**サンプル出力：**
```fish
42
```

**任意の2つの数値、例えば50と150の間でランダムな数値を生成する：**

```fish
set -l min 50
set -l max 150
set -l rand_num (random $min $max)
echo $rand_num
```

**サンプル出力：**
```fish
103
```

**リストをシャッフルするためにrandomを使用する：**

リスト内の要素をランダムにシャッフルしたい場合もあるでしょう。こうする方法は次のとおりです：

```fish
set -l my_list A B C D E
random (seq (count $my_list)) | while read i
    echo $my_list[$i]
end
```

**サンプル出力：**
```fish
C
A
E
D
B
```

これらのコマンドを実行するたびに出力は変わることに注意してください。これはランダム性の性質によるものです。

## 詳細な調査
Fish Shellの`random`関数は、疑似乱数を生成するための使いやすいインターフェースを提供します。内部では、システムレベルの乱数生成ユーティリティをラップしており、スクリプトにランダム性を導入するための携帯性の高い方法を提供します。ただし、`random`によって提供されるランダム性はほとんどのスクリプトタスクには十分ですが、高い予測不可能性を必要とするアプリケーションにおいて、暗号学的なセキュリティ要件を満たす可能性はありません。

高度なセキュリティコンテキストにおいては、より強力なランダム性の保証を提供する、暗号学的目的のために設計された専用ツールやプログラミングライブラリの使用を検討してください。それでも、最高のセキュリティ標準のランダム性が必要とされない一般的なスクリプティングやアプリケーションにおいては、Fish Shellの`random`機能は便利で効果的な解決策を提供します。

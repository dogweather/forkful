---
date: 2024-01-26 03:37:35.850322-07:00
description: "\u65B9\u6CD5 \u305F\u3068\u3048\u3070\u3001\u9577\u3055\u3068\u5E45\u304C\
  \u4E0E\u3048\u3089\u308C\u305F\u77E9\u5F62\u306E\u9762\u7A4D\u3068\u5468\u56F2\u3092\
  \u8A08\u7B97\u3057\u3066\u5370\u5237\u3059\u308B\u30B3\u30FC\u30C9\u306E\u584A\u304C\
  \u3042\u3063\u305F\u3068\u3057\u307E\u3059\u3002\u305D\u308C\u306F\u4ED5\u4E8B\u3092\
  \u3057\u307E\u3059\u304C\u3001\u7E70\u308A\u8FD4\u3057\u3067\u5C11\u3057\u96D1\u7136\
  \u3068\u3057\u3066\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.511339-06:00'
model: gpt-4-0125-preview
summary: "\u305F\u3068\u3048\u3070\u3001\u9577\u3055\u3068\u5E45\u304C\u4E0E\u3048\
  \u3089\u308C\u305F\u77E9\u5F62\u306E\u9762\u7A4D\u3068\u5468\u56F2\u3092\u8A08\u7B97\
  \u3057\u3066\u5370\u5237\u3059\u308B\u30B3\u30FC\u30C9\u306E\u584A\u304C\u3042\u3063\
  \u305F\u3068\u3057\u307E\u3059\u3002\u305D\u308C\u306F\u4ED5\u4E8B\u3092\u3057\u307E\
  \u3059\u304C\u3001\u7E70\u308A\u8FD4\u3057\u3067\u5C11\u3057\u96D1\u7136\u3068\u3057\
  \u3066\u3044\u307E\u3059."
title: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0"
weight: 19
---

## 方法
たとえば、長さと幅が与えられた矩形の面積と周囲を計算して印刷するコードの塊があったとします。それは仕事をしますが、繰り返しで少し雑然としています。

```python
# オリジナルバージョン
length = 4
width = 3

# 面積と周囲を計算
area = length * width
perimeter = 2 * (length + width)

print("面積:", area)
print("周囲:", perimeter)
```

機能を関数にカプセル化することで、このコードをリファクタリングできます。これにより、コードはより整理され再利用可能になります：

```python
# リファクタリングバージョン

def calculate_area(length, width):
    return length * width

def calculate_perimeter(length, width):
    return 2 * (length + width)

# 使用法
length = 4
width = 3

print("面積:", calculate_area(length, width))
print("周囲:", calculate_perimeter(length, width))
```

両方のスニペットは同じ結果を出力します：
```
面積: 12
周囲: 14
```

しかし、リファクタリングされたバージョンはよりクリーンで関心を分離しており、一方の計算を更新しても他方に影響を与えずに済むようにします。

## 深掘り
リファクタリングは、プログラマーがコードが「動作中」であっても改善できる、そしてすべきであることに気づいたソフトウェア工学の初期の日々にルーツがあります。マーティン・ファウラーの画期的な書籍「リファクタリング：既存のコードの設計を改善する」は、多くの核心原則と技術を明確に述べました。彼は有名に「どんな愚か者でもコンピュータが理解できるコードを書ける。良いプログラマは人間が理解できるコードを書く」と言いました。

リファクタリングの代替手段には、ゼロからコードを書き直すことや、体系的な改善なしに小規模な調整を加えることが含まれます。しかし、リファクタリングは通常、書き直しよりもコスト効果が高く、アドホックな修正よりもリスクが低いです。実装の詳細は各プログラミングパラダイムに固有のものかもしれませんが、オブジェクト指向プログラミングは、特に関数の抽出（私たちの`calculate_area`関数と`calculate_perimeter`関数のような）、インライン化、オブジェクト間での機能の移動、そして明確性のためのメソッドや変数の名前の変更などの技術で、リファクタリングに特に適しています。

Pythonでのリファクタリングは、組み込みのリファクタリング能力を備えた`PyCharm`や、リファクタリングに特化して設計されたPythonライブラリである`rope`などのツールをよく使用します。リファクタリング中には、`git`のようなバージョン管理を注意深く使用し、変更を段階的に追跡することが強く推奨されます。

## 参照
もっと詳しく知りたい方は、以下のリソースをご覧ください：
- マーティン・ファウラーの本：[リファクタリング：既存のコードの設計を改善する](http://www.refactoring.com/)
- Pythonでのリファクタリング `rope`：[GitHub - rope](https://github.com/python-rope/rope)
- PyCharm リファクタリングドキュメント：[Jetbrains PyCharm リファクタリングソースコード](https://www.jetbrains.com/help/pycharm/refactoring-source-code.html)
- Refactoring.guru：[リファクタリングとデザインパターン](https://refactoring.guru/refactoring)
- Uncle Bob（ロバートC.マーティン）によるクリーンコード講義：[クリーンコード - Uncle Bob / レッスン 1](https://www.youtube.com/watch?v=7EmboKQH8lM)

---
title:                "標準エラーへの書き込み"
html_title:           "Swift: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 何をしているのか & なぜ？ 
標準エラーへの書き込みとは何か、そしてプログラマーがこれをする理由について2〜3文で説明します。

## 方法： 
「Swift ...」コードブロック内にコーディング例とサンプルの出力を記載します。

```Swift
//例：
print("エラーが発生しました", to: &stderr)
```

## 深堀り： 
標準エラーへの書き込みの歴史的背景や代替手段、実装の詳細など、より深く理解するための情報を紹介します。

## 関連情報： 
関連するソースへのリンクを記載します。

- [Stack Overflow](https://stackoverflow.com/questions/29712371/how-to-print-output-to-error-console-swift)：標準エラーへの書き込み方法についてのコミュニティの質問と回答があります。
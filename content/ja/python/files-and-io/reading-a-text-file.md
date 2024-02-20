---
date: 2024-01-20 17:55:08.077447-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\u3053\
  \u3068\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u306E\u5185\u5BB9\u3092\u6587\u5B57\u5217\
  \u3068\u3057\u3066Python\u306B\u53D6\u308A\u8FBC\u3080\u3053\u3068\u3067\u3059\u3002\
  \u3053\u308C\u3092\u884C\u3046\u7406\u7531\u306F\u3001\u30C7\u30FC\u30BF\u5206\u6790\
  \u3001\u8A2D\u5B9A\u306E\u8AAD\u307F\u8FBC\u307F\u3001\u30ED\u30B0\u306E\u78BA\u8A8D\
  \u306A\u3069\u3001\u60C5\u5831\u3092\u5229\u7528\u3057\u305F\u3044\u591A\u304F\u306E\
  \u5834\u9762\u304C\u3042\u308B\u304B\u3089\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:00.795382
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\u3053\
  \u3068\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u306E\u5185\u5BB9\u3092\u6587\u5B57\u5217\
  \u3068\u3057\u3066Python\u306B\u53D6\u308A\u8FBC\u3080\u3053\u3068\u3067\u3059\u3002\
  \u3053\u308C\u3092\u884C\u3046\u7406\u7531\u306F\u3001\u30C7\u30FC\u30BF\u5206\u6790\
  \u3001\u8A2D\u5B9A\u306E\u8AAD\u307F\u8FBC\u307F\u3001\u30ED\u30B0\u306E\u78BA\u8A8D\
  \u306A\u3069\u3001\u60C5\u5831\u3092\u5229\u7528\u3057\u305F\u3044\u591A\u304F\u306E\
  \u5834\u9762\u304C\u3042\u308B\u304B\u3089\u3067\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
テキストファイルを読むことは、ファイルの内容を文字列としてPythonに取り込むことです。これを行う理由は、データ分析、設定の読み込み、ログの確認など、情報を利用したい多くの場面があるからです。

## How to: (方法)
```Python
# テキストファイルを読む一番シンプルな方法
with open('example.txt', 'r') as file:
    content = file.read()
print(content)
```

```Python
# ファイルを行ごとに読む
with open('example.txt', 'r') as file:
    for line in file:
        print(line.strip())  # strip()で行末の改行を削除
```

```
# 出力例:
こんにちは、Python!
ファイルを読むのは簡単です。
```

## Deep Dive (深掘り)
歴史的には、ファイル読み込みにはいくつかの方法がありました。旧バージョンのPythonでは`file`オブジェクト直接を使ったが、Python 2.5で`with`文が導入され、`open`関数と組み合わせてリソースの管理が安全になりました。`read`, `readline`, または `readlines`メソッドを使うと多様な読み込み方ができます。実装の詳細としては、Pythonはファイルシステムとオペレーティングシステムの違いから隠蔽層を提供し、開発者が簡単にファイルを取り扱えるようになっています。もっと大きなファイルやバイナリデータの場合は`io`モジュールの`BytesIO`や`StringIO`を使う代替方法があります。

## See Also (参考)
- 公式PythonドキュメントのファイルI/O: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Pythonの`open`関数の詳細: https://docs.python.org/3/library/functions.html#open
- `io`モジュールについての公式ドキュメント: https://docs.python.org/3/library/io.html

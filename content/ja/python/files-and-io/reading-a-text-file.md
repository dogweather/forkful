---
date: 2024-01-20 17:55:08.077447-07:00
description: "How to: (\u65B9\u6CD5) \u6B74\u53F2\u7684\u306B\u306F\u3001\u30D5\u30A1\
  \u30A4\u30EB\u8AAD\u307F\u8FBC\u307F\u306B\u306F\u3044\u304F\u3064\u304B\u306E\u65B9\
  \u6CD5\u304C\u3042\u308A\u307E\u3057\u305F\u3002\u65E7\u30D0\u30FC\u30B8\u30E7\u30F3\
  \u306EPython\u3067\u306F`file`\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u76F4\u63A5\u3092\
  \u4F7F\u3063\u305F\u304C\u3001Python 2.5\u3067`with`\u6587\u304C\u5C0E\u5165\u3055\
  \u308C\u3001`open`\u95A2\u6570\u3068\u7D44\u307F\u5408\u308F\u305B\u3066\u30EA\u30BD\
  \u30FC\u30B9\u306E\u7BA1\u7406\u304C\u5B89\u5168\u306B\u306A\u308A\u307E\u3057\u305F\
  \u3002`read`, `readline`,\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.473990-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u6B74\u53F2\u7684\u306B\u306F\u3001\u30D5\u30A1\u30A4\u30EB\
  \u8AAD\u307F\u8FBC\u307F\u306B\u306F\u3044\u304F\u3064\u304B\u306E\u65B9\u6CD5\u304C\
  \u3042\u308A\u307E\u3057\u305F\u3002\u65E7\u30D0\u30FC\u30B8\u30E7\u30F3\u306EPython\u3067\
  \u306F`file`\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u76F4\u63A5\u3092\u4F7F\u3063\u305F\
  \u304C\u3001Python 2.5\u3067`with`\u6587\u304C\u5C0E\u5165\u3055\u308C\u3001`open`\u95A2\
  \u6570\u3068\u7D44\u307F\u5408\u308F\u305B\u3066\u30EA\u30BD\u30FC\u30B9\u306E\u7BA1\
  \u7406\u304C\u5B89\u5168\u306B\u306A\u308A\u307E\u3057\u305F\u3002`read`, `readline`,\
  \ \u307E\u305F\u306F `readlines`\u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u3046\u3068\
  \u591A\u69D8\u306A\u8AAD\u307F\u8FBC\u307F\u65B9\u304C\u3067\u304D\u307E\u3059\u3002\
  \u5B9F\u88C5\u306E\u8A73\u7D30\u3068\u3057\u3066\u306F\u3001Python\u306F\u30D5\u30A1\
  \u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\u3068\u30AA\u30DA\u30EC\u30FC\u30C6\u30A3\u30F3\
  \u30B0\u30B7\u30B9\u30C6\u30E0\u306E\u9055\u3044\u304B\u3089\u96A0\u853D\u5C64\u3092\
  \u63D0\u4F9B\u3057\u3001\u958B\u767A\u8005\u304C\u7C21\u5358\u306B\u30D5\u30A1\u30A4\
  \u30EB\u3092\u53D6\u308A\u6271\u3048\u308B\u3088\u3046\u306B\u306A\u3063\u3066\u3044\
  \u307E\u3059\u3002\u3082\u3063\u3068\u5927\u304D\u306A\u30D5\u30A1\u30A4\u30EB\u3084\
  \u30D0\u30A4\u30CA\u30EA\u30C7\u30FC\u30BF\u306E\u5834\u5408\u306F`io`\u30E2\u30B8\
  \u30E5\u30FC\u30EB\u306E`BytesIO`\u3084`StringIO`\u3092\u4F7F\u3046\u4EE3\u66FF\u65B9\
  \u6CD5\u304C\u3042\u308A\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
weight: 22
---

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

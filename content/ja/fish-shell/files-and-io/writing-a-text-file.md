---
aliases:
- /ja/fish-shell/writing-a-text-file/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:59.794609-07:00
description: "Fish Shell\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306B\
  \u66F8\u304D\u8FBC\u3080\u3053\u3068\u3067\u3001\u30C7\u30FC\u30BF\u3092\u6C38\u7D9A\
  \u7684\u306B\u4FDD\u5B58\u3057\u3001\u540C\u3058Fish\u30B9\u30AF\u30EA\u30D7\u30C8\
  \u3084\u4ED6\u306E\u30D7\u30ED\u30B0\u30E9\u30E0\u306B\u3088\u308B\u30C7\u30FC\u30BF\
  \u306E\u7C21\u5358\u306A\u53D6\u5F97\u3084\u64CD\u4F5C\u3092\u53EF\u80FD\u306B\u3057\
  \u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30ED\u30B0\u306E\
  \u8A18\u9332\u3001\u8A2D\u5B9A\u306E\u4FDD\u5B58\u3001\u307E\u305F\u306F\u3055\u3089\
  \u306A\u308B\u51E6\u7406\u306E\u305F\u3081\u306E\u30C7\u30FC\u30BF\u306E\u30A8\u30AF\
  \u30B9\u30DD\u30FC\u30C8\u306A\u3069\u306E\u76EE\u7684\u3067\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:55.326382
model: gpt-4-0125-preview
summary: "Fish Shell\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306B\u66F8\
  \u304D\u8FBC\u3080\u3053\u3068\u3067\u3001\u30C7\u30FC\u30BF\u3092\u6C38\u7D9A\u7684\
  \u306B\u4FDD\u5B58\u3057\u3001\u540C\u3058Fish\u30B9\u30AF\u30EA\u30D7\u30C8\u3084\
  \u4ED6\u306E\u30D7\u30ED\u30B0\u30E9\u30E0\u306B\u3088\u308B\u30C7\u30FC\u30BF\u306E\
  \u7C21\u5358\u306A\u53D6\u5F97\u3084\u64CD\u4F5C\u3092\u53EF\u80FD\u306B\u3057\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30ED\u30B0\u306E\u8A18\
  \u9332\u3001\u8A2D\u5B9A\u306E\u4FDD\u5B58\u3001\u307E\u305F\u306F\u3055\u3089\u306A\
  \u308B\u51E6\u7406\u306E\u305F\u3081\u306E\u30C7\u30FC\u30BF\u306E\u30A8\u30AF\u30B9\
  \u30DD\u30FC\u30C8\u306A\u3069\u306E\u76EE\u7684\u3067\u3053\u308C\u3092\u884C\u3044\
  \u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## 何となぜ？

Fish Shellでテキストファイルに書き込むことで、データを永続的に保存し、同じFishスクリプトや他のプログラムによるデータの簡単な取得や操作を可能にします。プログラマーは、ログの記録、設定の保存、またはさらなる処理のためのデータのエクスポートなどの目的でこれを行います。

## 方法：

Fishでテキストファイルに書き込むには、`echo`コマンドとリダイレクションオペレーターを組み合わせて使用します。ファイル書き込み用の特に人気のあるサードパーティ製ライブラリはFishにはありませんが、シェルの組み込みコマンドはこの目的に対して直接的で効率的です。

### 新しいファイルにテキストを書き込むか、既存のファイルを上書きする：
```fish
echo "Hello, Fish Shell!" > output.txt
```
このコマンドは"Hello, Fish Shell!" を `output.txt` に書き込み、ファイルが存在しない場合は作成し、存在する場合は上書きします。

### 既存のファイルにテキストを追加する：
現在の内容を削除せずに既存のファイルの末尾にテキストを追加するには、追記オペレーター `>>` を使用します：
```fish
echo "Adding new line to file." >> output.txt
```

### 複数行の書き込み：
改行文字 `\n` を使用してechoで複数行をファイルに書き込むか、またはセミコロンを使用して複数のechoコマンドを連鎖させることができます：
```fish
echo "First Line\nSecond Line" > output.txt
# または
echo "First Line" > output.txt; echo "Second Line" >> output.txt
```

### サンプル出力：
上記のコマンドを実行した後の `output.txt` の内容を表示するには、`cat` コマンドを使用します：
```fish
cat output.txt
```
```plaintext
First Line
Second Line
```
示されたようにテキストを置換するか追加することで、要件に応じてファイルの内容を操作することができ、Fish Shellでテキストファイルを扱うシンプルかつ強力な方法を示しています。

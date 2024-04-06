---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:59.794609-07:00
description: "\u65B9\u6CD5\uFF1A Fish\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\
  \u30EB\u306B\u66F8\u304D\u8FBC\u3080\u306B\u306F\u3001`echo`\u30B3\u30DE\u30F3\u30C9\
  \u3068\u30EA\u30C0\u30A4\u30EC\u30AF\u30B7\u30E7\u30F3\u30AA\u30DA\u30EC\u30FC\u30BF\
  \u30FC\u3092\u7D44\u307F\u5408\u308F\u305B\u3066\u4F7F\u7528\u3057\u307E\u3059\u3002\
  \u30D5\u30A1\u30A4\u30EB\u66F8\u304D\u8FBC\u307F\u7528\u306E\u7279\u306B\u4EBA\u6C17\
  \u306E\u3042\u308B\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u88FD\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u306FFish\u306B\u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001\u30B7\
  \u30A7\u30EB\u306E\u7D44\u307F\u8FBC\u307F\u30B3\u30DE\u30F3\u30C9\u306F\u3053\u306E\
  \u76EE\u7684\u306B\u5BFE\u3057\u3066\u76F4\u63A5\u7684\u3067\u52B9\u7387\u7684\u3067\
  \u3059\u3002"
lastmod: '2024-04-05T22:38:42.245193-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Fish\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\
  \u306B\u66F8\u304D\u8FBC\u3080\u306B\u306F\u3001`echo`\u30B3\u30DE\u30F3\u30C9\u3068\
  \u30EA\u30C0\u30A4\u30EC\u30AF\u30B7\u30E7\u30F3\u30AA\u30DA\u30EC\u30FC\u30BF\u30FC\
  \u3092\u7D44\u307F\u5408\u308F\u305B\u3066\u4F7F\u7528\u3057\u307E\u3059\u3002\u30D5\
  \u30A1\u30A4\u30EB\u66F8\u304D\u8FBC\u307F\u7528\u306E\u7279\u306B\u4EBA\u6C17\u306E\
  \u3042\u308B\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u88FD\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u306FFish\u306B\u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001\u30B7\u30A7\
  \u30EB\u306E\u7D44\u307F\u8FBC\u307F\u30B3\u30DE\u30F3\u30C9\u306F\u3053\u306E\u76EE\
  \u7684\u306B\u5BFE\u3057\u3066\u76F4\u63A5\u7684\u3067\u52B9\u7387\u7684\u3067\u3059\
  \u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 24
---

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

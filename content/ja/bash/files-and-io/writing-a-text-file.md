---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:35.346576-07:00
description: "\u65B9\u6CD5\uFF1A Bash\u306F\u30D5\u30A1\u30A4\u30EB\u3078\u306E\u66F8\
  \u304D\u8FBC\u307F\u306E\u305F\u3081\u306E\u76F4\u63A5\u7684\u306A\u65B9\u6CD5\u3092\
  \u63D0\u4F9B\u3057\u307E\u3059\u3002\u6700\u3082\u4E00\u822C\u7684\u306A\u306E\u306F\
  \u3001\u30EA\u30C0\u30A4\u30EC\u30AF\u30B7\u30E7\u30F3\u6F14\u7B97\u5B50\uFF08`>`\u3001\
  `>>`\uFF09\u3068`tee`\u30B3\u30DE\u30F3\u30C9\u306E\u4F7F\u7528\u3067\u3059\u3002\
  \u4E21\u65B9\u306E\u6280\u8853\u3092\u7C21\u5358\u306B\u898B\u3066\u307F\u307E\u3057\
  \u3087\u3046\u3002 \u30EA\u30C0\u30A4\u30EC\u30AF\u30B7\u30E7\u30F3\u3092\u4F7F\u7528\
  \u3057\u3066\u3001\u51FA\u529B\u3092\u76F4\u63A5\u30D5\u30A1\u30A4\u30EB\u306B\u66F8\
  \u304D\u8FBC\u3080\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002`>`\u2026"
lastmod: '2024-04-05T21:53:43.228956-06:00'
model: gpt-4-0125-preview
summary: "\u30EA\u30C0\u30A4\u30EC\u30AF\u30B7\u30E7\u30F3\u3092\u4F7F\u7528\u3057\
  \u3066\u3001\u51FA\u529B\u3092\u76F4\u63A5\u30D5\u30A1\u30A4\u30EB\u306B\u66F8\u304D\
  \u8FBC\u3080\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002`>` \u6F14\u7B97\u5B50\
  \u306F\u3001\u65E2\u306B\u5B58\u5728\u3059\u308B\u5834\u5408\u306F\u305D\u308C\u3092\
  \u7F6E\u304D\u63DB\u3048\u3064\u3064\u30D5\u30A1\u30A4\u30EB\u306B\u30B3\u30F3\u30C6\
  \u30F3\u30C4\u3092\u66F8\u304D\u8FBC\u307F\u307E\u3059\u304C\u3001`>>` \u306F\u65E2\
  \u5B58\u306E\u30D5\u30A1\u30A4\u30EB\u306B\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\u8FFD\
  \u52A0\u3057\u3001\u305D\u306E\u5185\u5BB9\u3092\u524A\u9664\u305B\u305A\u306B\u8FFD\
  \u52A0\u3057\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 24
---

## 方法：
Bashはファイルへの書き込みのための直接的な方法を提供します。最も一般的なのは、リダイレクション演算子（`>`、`>>`）と`tee`コマンドの使用です。両方の技術を簡単に見てみましょう。

リダイレクションを使用して、出力を直接ファイルに書き込むことができます。`>` 演算子は、既に存在する場合はそれを置き換えつつファイルにコンテンツを書き込みますが、`>>` は既存のファイルにコンテンツを追加し、その内容を削除せずに追加します。

```bash
# > を使ってファイルに書き込む
echo "Hello, World!" > myfile.txt

# >> を使ってファイルに追加する
echo "This is a new line." >> myfile.txt
```

上記のコマンドを実行した後に `myfile.txt` の内容をチェックすると、以下が見つかります：

```
Hello, World!
This is a new line.
```

ファイルに書き込みつつ、出力を画面（stdout）上でも見たい場合は、`tee` コマンドが便利です。デフォルトでは、`tee` はファイルを上書きしますが、`-a` フラグを使用すると、ファイルに追記されます。

```bash
# tee を使用して書き込み・表示
echo "Hello, again!" | tee myfile.txt

# tee -a を使用して追加・表示
echo "Adding another line." | tee -a myfile.txt
```

これらを実行した後、`myfile.txt` は次のように表示されます：

```
Hello, again!
Adding another line.
```

Bash自身がリダイレクションや`tee`のようなコマンドを通して強力なファイル操作能力を提供している一方で、さらなる操作や複雑なシナリオには外部ツールやスクリプト言語（例：Awk、Sed、Python）を呼び出すことが必要かもしれません。これらはより洗練されたテキスト処理機能を提供します。しかし、最も単純なファイル書き込みタスクには、上記の方法で十分であり、広く使用されています。

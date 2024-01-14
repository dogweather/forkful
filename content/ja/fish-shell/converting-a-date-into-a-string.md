---
title:    "Fish Shell: 日付を文字列に変換する"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換することに興味がある人々はたくさんいますが、Fish Shellを使ったプログラミングを行い、日付をより柔軟に操作するためには、日付を文字列に変換する必要があります。この記事では、このプロセスを簡単にご紹介します。

## 方法
まず、`date`コマンドを使用して今日の日付を取得します。

```
Fish Shell » date +%D
02/15/22
```

その後、`string`コマンドを使用して上記の日付を文字列に変換します。

```
Fish Shell » echo (date +%D) | string split /;
02 15 22
```

この例では、`date`コマンドの出力をパイプで`string`コマンドに渡し、`split`オプションを使用してスラッシュ（/）で分割しました。これにより、月、日、年がそれぞれ個々の要素となる配列が作成されます。

また、`string`コマンドを使用する際は、`replace`オプションを使用して特定の文字列を置き換えることもできます。

```
Fish Shell » echo (date +%D) | string replace / -
02-15-22
```

このように、`string`コマンドを使用することで、日付をより細かく操作することができます。

## ディープダイブ
日付を文字列に変換する際に、より詳細に理解したい方のために、少し深く掘り下げてみましょう。

まず、`date`コマンドを使用する際に、`+%D`オプションを使用することで、月、日、年のフォーマットで日付を取得することができます。また、`+%d`オプションを使用することで、先頭にゼロをつけない日付を取得することができます。

さらに、`string`コマンドを使用する際には、`find -r`オプションを使用することで、正規表現を使用して特定のパターンを検索することができます。

```
Fish Shell » echo (date +%D) | string find -r [0-9]+
02 15 22
```

このように、このオプションを使用することで、より複雑な操作も可能になります。

## 関連情報
この記事では、日付を文字列に変換する方法についてご紹介しましたが、他にもFish Shellに関する素晴らしい情報がたくさんあります。ぜひ以下のリンクをご参照ください！

* [公式ドキュメント](https://fishshell.com/docs/current/index.html)
* [コミュニティフォーラム](https://github.com/fish-shell/fish-shell/issues)
* [プラグイン](https://github.com/oh-my-fish/oh-my-fish)
* [チュートリアル動画](https://www.youtube.com/watch?v=9hTcimZ-WvQ)

---
See Also

* [Date Command](https://www.unix.com/man-page/stev/1/date/)
* [String Command](https://www.unix.com/man-page/stev/1/string/)
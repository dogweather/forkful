---
title:                "テキストファイルの作成"
html_title:           "Haskell: テキストファイルの作成"
simple_title:         "テキストファイルの作成"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なんでやるの？

テキストファイルを書くことのメリットは多岐にわたります。例えば、データを保存しやすく、他のプログラムやシステムとの連携もしやすくなります。さらに、テキストファイルはプログラム言語ではないので、簡単に読み書きすることができます。

## 要約するとどうやるの？

以下のようなコードを `test.hs` というファイルで作成し、ターミナルで `runhaskell test.hs` と実行します。

```Haskell
main = do
  writeFile "テキストファイル.txt" "こんにちは、世界！"
```

これで、テキストファイル `テキストファイル.txt` が作成され、中身には `こんにちは、世界！` という文字列が書き込まれます。

## さらに詳しく見てみよう

テキストファイルを作成するためには、`writeFile` 関数を使用します。`writeFile` は2つの引数を受け取ります。1つ目の引数はファイルのパスや名前を指定する文字列で、2つ目の引数は書き込む内容を指定する文字列です。

また、テキストファイルを読み込むためには `readFile` 関数を使用します。`readFile` も2つの引数を受け取りますが、書き込む内容ではなく読み込むファイルのパスや名前を指定する文字列です。

`writeFile` や `readFile` を使うときは、適切なエラーハンドリングを行うことも重要です。詳しくは公式ドキュメントなどを読んでみてください。

## さらに詳しい記事を読んでみよう

- [Haskell 公式ドキュメント](https://www.haskell.org/documentation/)
- [Haskell 入門](https://qiita.com/7shi/items/145f123a1b08a6cf6a02)
- [Haskell でテキストファイルを読み書きする方法](https://qiita.com/7shi/items/7a87ca40803b0be2b5f9) 

## 関連記事を読んでみよう

- [Markdown を使って文書を書く](https://qiita.com/tbpgr/items/989c6badefff69377da7)
- [便利なテキストエディター TextMate 2 の使い方](https://qiita.com/amukox/items/ad2e78a1d946c68acfdc)
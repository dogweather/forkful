---
title:                "テキストファイルの読み込み"
date:                  2024-01-20T17:54:54.930369-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストファイルの読み込み"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
プログラムでテキストファイルを読むってのは、ファイルの内容をプログラムに取り込むことだ。データを処理したり、設定を読み込んだりするためにやるのが普通さ。

## How to: (やり方)
```Gleam
import gleam/io
import gleam/result

pub fn main() {
  let result = io.read_file("your_file.txt") // ファイル名を変えてね
  case result {
    Ok(contents) ->
      // 読めた時の処理
      io.println(contents)
    Error(err) ->
      // エラー時の処理
      io.println("Oops! We got an error: ")
      io.println(err)
  }
}
```
出力例:
```
Hello from your_file.txt!
```

## Deep Dive (深掘り)
昔はファイルを読むのにもっと低レベルの操作が必要だったけど、今はいろんな言語に簡単な関数が用意されてる。Gleamでは`io.read_file/1`関数を使うのが基本。`Result(String, Error)`を返すから、内容が取れるかエラーかすぐわかる。`Ok`ならデータが、`Error`なら問題の詳細が手に入る。

同様にGleamには`io.write_file/2`もあって、ファイルに書き込みたい時に使える。ファイルを読むのと書くのは表裏一体だな。

他の方法としては、ストリームで読み込む方式がある。これは何ギガもあるファイルに有効で、メモリに優しい。Gleamだと、まだ実験段階だが、いずれこういう機能が標準で出るかもしれない。

## See Also (参照)
- Gleam公式のIOドキュメント: https://hexdocs.pm/gleam_stdlib/gleam/io/
- GleamのGitHubリポジトリ: https://github.com/gleam-lang/gleam
- ファイルのストリーム処理に関する議論: https://github.com/gleam-lang/suggestions/issues
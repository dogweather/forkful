---
title:                "Ruby: 「テキストファイルの読み込み」"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを読み込むことは、プログラミングの世界で非常に一般的な作業です。テキストファイルを読み込むことで、データを素早く処理し、より複雑なプログラムを作成することができます。

## 方法

まずは、Rubyでファイルを開いて読み込む基本的な方法を紹介します。最初にファイルをオープンし、その後ファイルを1行ずつ読み込んで処理していきます。その際、「readline」メソッドを使用することで、ファイル内のデータを1行ずつ取得することができます。

```Ruby
file = File.open("sample.txt", "r")

file.each do |line|
  # ファイルの各行に対して処理を行う
  puts line
end

file.close
```

上記の例では、ファイルを開く際、"r"という引数を指定しています。これは、ファイルを読み込みモードで開くことを意味します。また、ファイルを開いた後は、必ず「close」メソッドを使用してファイルを閉じるようにしましょう。

## ディープダイブ

ファイルを読み込む際には、データをどのような形式で扱うかが重要です。Rubyでは、テキストファイル内のデータを配列やハッシュの形で処理することができます。また、正規表現を使用することで、特定のパターンに一致する行のみを抽出することができます。

さらに、ファイルの読み込み中にエラーが発生した場合は、例外処理を行うことも重要です。例外処理を使用することで、プログラムが予期しないエラーに対しても安全に対応することができます。

## See Also

- [Rubyでファイルを扱う方法](https://qiita.com/masky808/items/cf18427cd800ab1b6f11)
- [Rubyの正規表現について](https://qiita.com/jnchito/items/893c887fbf19e17d3ff9)
- [Rubyにおける例外処理の基本](https://qiita.com/shikichee/items/bba576cc73e7ae43fa41)
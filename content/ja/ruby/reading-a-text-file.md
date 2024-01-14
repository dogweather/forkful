---
title:                "Ruby: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

# なぜ？

テキストファイルを読み取ることの重要性は、プログラミング言語を学ぶ上で必要不可欠です。テキストファイルを読めるようになると、データの処理や分析をより効率的に行うことができ、開発プロジェクトにおいて非常に役立ちます。

# 使い方

テキストファイルを読み取るには、Rubyに付属しているFileクラスを使用します。まず、読み取りたいファイルを指定し、`read`メソッドを使用してファイルの内容を読み込みます。その後、ループ処理を行い、ファイル内のデータを1行ずつ読み込むことができます。

```Ruby
file = File.open("sample.txt", "r")
file_data = file.read
file_data.each_line do |line|
  puts line
end

#=> 今日はいい天気です。
#=> 明日は雨が降りそうです。
#=> 傘を持って行ったほうがいいかもしれません。
```

# ディープダイブ

テキストファイルを読み取る際には、注意すべき点がいくつかあります。例えば、ファイルを開いた後は、必ず`close`メソッドを使用してファイルを閉じる必要があります。また、ファイルの内容を読み込む際には、文字コードの変換にも注意を払う必要があります。

さらに、テキストファイル以外にも、異なる形式のファイルを読み取る方法もあります。例えば、CSVファイルやJSONファイルなどの特定の形式に最適化されたメソッドを使用することで、より高度なデータ処理が可能になります。

# 参考リンク

- [Rubyドキュメンテーション](https://docs.ruby-lang.org/ja/3.0.0/class/File.html)
- [Rubyテキストファイルの読み書き方法のまとめ](https://qiita.com/egg_chicken/items/6b6caac475bfab5b3e78)
- [Rubyにおけるファイル入出力の基本](https://qiita.com/hayatelife/items/8310c4135486fd1758aa)
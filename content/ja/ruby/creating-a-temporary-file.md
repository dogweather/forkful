---
title:    "Ruby: 「一時ファイルの作成」"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

あなたがプログラミングを学んでいるならば、おそらく一度は一時ファイルを作成する必要性に遭遇したことがあるでしょう。一時ファイルは、プログラムがデータを保存するための一時的な場所です。ここでは、Rubyを使って一時ファイルを作成して保存する方法をご紹介します。

## 作り方

一時ファイルを作成するには、`Tempfile`クラスを使います。まずは、必要なライブラリを読み込みます。

```Ruby
require 'tempfile'
```

次に、`Tempfile.new`メソッドを使って一時ファイルを作成します。このメソッドは、引数にファイル名のプレフィックスを取ります。その後、ファイルを編集するために`open`メソッドを使います。

```Ruby
tempfile = Tempfile.new('sample')
tempfile.open
```

ファイルに書き込むには、`puts`メソッドを使います。

```Ruby
tempfile.puts "Hello, World!"
```

最後に、`close`メソッドを使ってファイルを閉じます。

```Ruby
tempfile.close
```

これで、一時ファイルが作成され、"Hello, World!"という文字列が書き込まれます。

## 深堀

一時ファイルを作成するときには、ファイルのオープンやクローズを手動で行う必要はありません。`Tempfile.open`メソッドを使うと、ファイルをオープンしてブロック内の処理が終了した後に自動的にクローズします。

また、作成した一時ファイルのパスを取得するには、`tempfile.path`メソッドを使います。その他のオプションや詳細な使い方については、公式ドキュメントを参照してください。

## さらに詳しく知りたい方へ

ここでは、一時ファイルを作成する方法について簡単に説明しましたが、実際には様々な方法があります。他にも、ファイルを削除する方法やファイルのモードを指定する方法など、さらに詳しく知りたい方は以下のリンクを参考にしてください。

- [Rubyドキュメント - Tempfileクラス](https://docs.ruby-lang.org/ja/latest/class/Tempfile.html)
- [一時ファイルの作成方法 | TechAcademyマガジン](https://techacademy.jp/magazine/32801)
- [一時ファイルを作成する方法 | Qiita](https://qiita.com/sukepachi/items/cabf9b1f368cc1531311)

## 関連記事を見る
---
title:                "日付を文字列に変換する"
html_title:           "Ruby: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換する必要があるのか？私たちが日々コンピューターを使う中で、データをより見やすく、扱いやすくする必要があります。日付のデータもその一つです。例えば、バックアップファイルの保存時に日付を文字列に変換し、ファイル名に付けて整理することで、ファイルの管理が簡単になります。

## 方法
日付を文字列に変換する方法は複数ありますが、Rubyの組み込みメソッドである`strftime`を使うのが一般的です。`strftime`は、日付を指定したフォーマットに従って文字列に変換してくれる優れたメソッドです。

```Ruby
# Dateオブジェクトを作成
date = Date.new(2021, 9, 10)

# strftimeを使って文字列に変換
p date.strftime("%Y年%m月%d日") #=> "2021年09月10日"
p date.strftime("%m/%d/%Y") #=> "09/10/2021"
p date.strftime("%b %d, %Y") #=> "Sep 10, 2021"
```

上記のように、`strftime`の引数には日付を表すフォーマットを指定することができます。より詳細なフォーマットについては、公式ドキュメントを参照してください。

## ディープダイブ
日付を文字列に変換する際、よく使われるフォーマットには以下のようなものがあります。

- `%Y`：4桁の西暦
- `%m`：0埋めされた月 (例：01, 09)
- `%d`：0埋めされた日 (例：07, 25)
- `%b`：月の省略名 (例：Jan, Sep)
- `%m`：日の省略名 (例：01, 10)
- `%H`：24時間表記の時
- `%I`：12時間表記の時
- `%M`：分
- `%S`：秒

これらのフォーマットを組み合わせて使うことで、さまざまな形式の日付を得ることができます。また、`strftime`にはより複雑なフォーマットも指定することができます。詳細については、公式ドキュメントを参照してください。

## 関連リンク
- [strftime メソッド - Rubyリファレンスマニュアル](https://ref.xaio.jp/ruby/classes/date/strftime)
- [Dateクラス - Rubyリファレンスマニュアル](https://ref.xaio.jp/ruby/classes/date)
- [Ruby言語公式サイト](https://www.ruby-lang.org/ja/)
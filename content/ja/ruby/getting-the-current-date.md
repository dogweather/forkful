---
title:                "現在の日付を取得する"
html_title:           "Ruby: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何 & なぜ?

「現在の日付を取得する」とはどのような意味か、そしてプログラマーがそれをする理由について説明します。

プログラマーは、プログラム内で正確な日付を取得することが重要です。例えば、ユーザーに登録した日付を表示したり、特定の日付を経過したかどうかをチェックしたりするためです。これにより、プログラムの機能性や正確性が向上します。

## 方法:

Rubyを使って現在の日付を取得する方法について説明します。

```ruby
# 現在の日付を取得する
current_date = Date.today
# yyyy-mm-ddの形式で出力する
puts current_date.strftime("%Y-%m-%d")
```

上記のコードを実行すると、現在の日付が「yyyy-mm-dd」の形式で出力されます。RubyのDateクラスを使って簡単に現在の日付を取得することができます。

## 詳細:

### 歴史的背景:

現在の日付の取得方法は、プログラミング言語によって異なります。早期のプログラミング言語では、日付や時間を取得するために専用の命令が必要でした。しかし、近年では多くのプログラミング言語が標準的な日付オブジェクトを持っており、簡単に現在の日付を取得できるようになりました。

### 代替方法:

Ruby以外のプログラミング言語でも、現在の日付を取得する方法はあります。例えば、Pythonでは「datetime」モジュールを使って同様のことができます。また、UnixやLinuxのシステムでは「date」コマンドを使って現在の日付を取得することができます。

### 実装の詳細:

Rubyで現在の日付を取得する際、内部的には「Time.now」というメソッドが走り、取得した日付情報を保持する「Date」というクラスのインスタンスを返します。つまり、Dateクラスを使っていますが、実際に取得しているのは時間の情報です。

## 関連情報:

- [RubyのDateクラスの公式ドキュメント](https://docs.ruby-lang.org/ja/latest/class/Date.html)
- [Pythonのdatetimeモジュールの公式ドキュメント](https://docs.python.org/ja/3/library/datetime.html)
- [UnixやLinuxで現在の日付を表示する方法](https://www.unix.com/man-page/linux/1/date/)
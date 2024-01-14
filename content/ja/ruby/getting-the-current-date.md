---
title:    "Ruby: 現在の日付を取得する"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

現在の日付を取得することについて、なぜ誰かが関わるべきかを1-2文で説明します。

現在の日付を取得することは、プログラミングの世界で非常に一般的なタスクです。多くのアプリケーションで、現在の日付が必要になるからです。例えば、アプリケーションのログに日付を記録するために必要になる場合があります。また、データの有効期限を設定する場合にも、現在の日付が必要になることがあります。どのような理由であれ、プログラマーとしては、現在の日付を取得する方法をマスターしておくことは重要です。

## 方法

現在の日付を取得するためには、Rubyの`Date`クラスを使用します。`today`メソッドを使用すると、現在の日付を取得することができます。以下のようにコードを記述します。

```Ruby 
today = Date.today
puts today
```

このコードを実行すると、以下のような出力が得られます。

``` 
2021-08-02
```

また、現在の日付だけでなく、特定の曜日や月を取得することもできます。以下のようにコードを記述します。

```Ruby 
require 'date'
puts Date.parse('1st Jan 2021') 
puts Date.parse('Sunday') 
```

このコードを実行すると、以下のような出力が得られます。

```
2021-01-01
2021-08-01
```

## ディープダイブ

現在の日付を取得する方法について、もう少し詳しく見ていきましょう。Rubyでは、`DateTime`クラスも利用することができます。`DateTime`は`Date`クラスよりも多機能で、日付だけでなく時刻の情報も取得することができます。

また、`DateTime`クラスには便利なメソッドがいくつかあります。例えば、`strftime`メソッドは日付を特定のフォーマットで表示することができます。例えば、以下のように`strftime`を使用すると、現在の日付を「月/日/年」の形式で表示することができます。

```Ruby 
today = DateTime.now
puts today.strftime("%m/%d/%Y")
```

このコードを実行すると、以下のような出力が得られます。

```
08/02/2021
```

## 詳しくは

今回はRubyで現在の日付を取得する方法についてご紹介しましたが、実は他にも様々な方法があります。例えば、`Time`や`Chronic`といったgemを使用する方法もあります。興味のある方は、ぜひ調べてみてください。

## 関連リンク

- [Dateクラスのドキュメント](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- [DateTimeクラスのドキュメント](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/DateTime.html)
- [RubyのTimeクラスを使った日付の操作方法](https://qiita.com/mogulla3/items/bd100d850f719f11d04a)
- [Chronic gemのドキュメント](https://github.com/mojombo/chronic)
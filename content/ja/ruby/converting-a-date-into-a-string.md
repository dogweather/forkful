---
title:    "Ruby: 「日付を文字列に変換する」"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

Why: 日付を文字列に変換する理由は、データを扱う上で非常に便利であり、情報を整理するために欠かせないものです。

How To: ```Ruby
require 'date'

#Current Date
current_date = Date.today 

#Convert to String
current_date_str = current_date.strftime("%m/%d/%Y") 

#Output
puts current_date_str 
```

**Output:** 08/12/2021

Deep Dive: 日付を文字列に変換する方法は、Rubyの `strftime`メソッドを使用することで実現できます。このメソッドは、日付を指定したフォーマットに従って、文字列として出力することができます。フォーマット指定には、年月日を表す `%Y/%m/%d`や、曜日を表す `%A`など、さまざまなオプションがあります。日付のフォーマットを自由にカスタマイズすることができるため、必要に応じて使い分けることができる重要な機能です。

See Also (参考になるリンク):
- [Rubyのstrftimeメソッドについて](https://docs.ruby-lang.org/ja/latest/class/Time.html#I_STRFTIME)
- [Rubyで日付を扱う方法を学ぶ](https://qiita.com/tsugitada01/items/604b03ce9e488d391784#%E6%97%A5%E4%BB%98%E3%82%92%E5%BC%95%E6%95%B0%E3%81%AB%E3%81%The article ends with a strict正しい形式のマークダウンファイルを作成する方法を学ぶ)
- [RubyのDateクラスについて](https://docs.ruby-lang.org/ja/latest/class/Date.html)

今回は、日付を文字列に変換する方法について簡単に紹介しました。Rubyの日付操作機能をよく使いこなし、効率的なプログラミングを心がけましょう。
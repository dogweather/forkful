---
title:    "Ruby: Begin your code2つの日付の比較"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## なぜ

日々のプログラムで、日付を比較することが必要になることがあります。例えば、予定を調整するために、ある日付が別の日付より前か後かを確認する必要がある場合などです。Rubyでは、日付を比較することが簡単にできるため、便利です。この記事では、日付を比較する方法をご紹介します。

## 使い方

Rubyでは、日付を比較するために `Date` クラスを使います。例えば、ある日付がある日付より後ろかどうかを判断するには、`>` や `<=` といった比較演算子を使います。また、日付を `Date.new` を使って作成することもできます。

 ```Ruby
 date1 = Date.new(2020, 7, 1)
 date2 = Date.new(2020, 7, 5)
 puts date2 > date1 # true
 puts date1 <= date2 # false
 ```
 
また、日付のフォーマットを変更したい場合は、`strftime` メソッドを使うことができます。
 
 ```Ruby
 date = Date.new(2020, 7, 1)
 puts date.strftime("%Y/%m/%d") # 2020/07/01
 ```
 
## 深堀り

日付を比較する際には、時刻の情報が無視されるため、日付のみが比較されます。また、`Date` クラスには様々なメソッドが用意されており、特定の日付が休日や平日などを判定することもできます。

また、日付の比較では同じ日付でも、時間や時間帯の情報が異なると「等しくない」と判定されることに注意が必要です。このような場合は、直接日時を比較する `DateTime` クラスを使うほうが適しています。

## さらに見る
 
- [RubyのDateクラス公式ドキュメント](https://docs.ruby-lang.org/ja/latest/class/Date.html)
- [Rubyで日時を扱う方法まとめ](https://qiita.com/katsuhisa__/items/4acd6460051233b01823)
- [Rubyでの日付と時刻の操作方法について](https://www.javadrive.jp/ruby/date_class/)
 
## 参考文献

この記事は、以下のリソースを参考に作成しました。
- [Rubyで日付を比較する方法](https://www.sejuku.net/blog/25336)
- [日付を比較する-「＜ =，> =」](https://docs.ruby-lang.org/ja/latest/class/Date.html#I_LESS_THAN_OR_EQUAL.3D.3E_)
- [Rubyで日付をフォーマットする方法](https://qiita.com/WorldWaiting/items/57c665bb51519b28a6b5)
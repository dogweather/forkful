---
title:    "Ruby: 「二つの日付の比較」"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ
日付を比較したいと思う理由はさまざまです。例えば、あるイベントの開始日と終了日を比較することで、イベントの期間を計算することができます。また、誕生日や記念日を比較することで、その日からどれだけ経ったかを計算することもできます。

## 使い方
比較したい日付を、RubyのDateオブジェクトに変換します。その後、比較演算子を使用して日付を比較することができます。例えば、あるイベントの開始日が"2021/01/01"で、終了日が"2021/01/15"の場合、以下のようなコードを記述します。

```Ruby
start_date = Date.new(2021, 01, 01)
end_date = Date.new(2021, 01, 15)
if start_date < end_date
  puts "イベントはまだ終わっていません"
else
  puts "イベントは終了しました"
end
```

コードの実行結果は、"イベントはまだ終わっていません"と表示されます。

## ディープダイブ
日付を比較する際に注意することがいくつかあります。まず、Rubyの日付はコンピューターの内部的な数値で表されています。そのため、日付を直接比較するのではなく、Dateオブジェクトを使用することで正確な比較ができます。また、時間の情報もDateオブジェクトに含めることができるので、時間も含めた比較が可能です。

## 関連リンクを見る
- [Rubyの日付を比較する方法についての記事](https://qiita.com/hiyuzawa/items/0a0d2b867a1cba207be5)
- [RubyのDateクラスの公式ドキュメント](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Rubyの比較演算子についての説明](https://www.javadrive.jp/ruby/ope/index6.html)
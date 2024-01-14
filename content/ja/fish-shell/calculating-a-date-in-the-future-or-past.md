---
title:    "Fish Shell: 未来や過去の日付を計算する。"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

#なぜ

前後の日付を計算することに興味がある人は多いかもしれません。例えば、予定を立てたり、期限を設定したりするために、未来の日付を知りたい場合があります。または、過去の日付を調べる必要があるかもしれません。Fish Shellを使用してこの計算を行うことは非常に便利です。

#やり方

Fish Shellのインストールがまだの場合は、まずそれを行ってください。次に、組み込みの`date`コマンドを使用して、指定された日付やタイムスタンプから日付を計算することができます。例えば、`date +%Y年%m月%d日 -d "5 days ago"`と入力すると、5日前の日付が表示されます。また、任意の日付やタイムスタンプを指定することもできます。

```Fish Shell
# 5日後の日付を計算する
echo (date +%Y年%m月%d日 -d "5 days from now")

# 任意の日付を計算する
echo (date +%Y年%m月%d日 -d "2021/12/25")
```

上記のコマンドを実行すると、次のような出力が得られます。

> 5 days ago: 2021年06月06日
>
 > 5 days from now: 2021年06月16日
> 
> 2021/12/25: 2021年12月25日

# 詳細を掘り下げる

`date`コマンドにはさまざまなオプションがあり、これを使用することでさらに多くの計算が可能です。例えば、特定の曜日の日付や、指定した日付よりも前または後の最も近い特定の曜日の日付を計算することもできます。また、タイムスタンプの場合は秒単位まで計算することもできます。

詳細なオプションについては、[公式ドキュメント](https://fishshell.com/docs/current/cmds/date.html)を参照してください。

# 参考

- [Fish Shell公式サイト](https://fishshell.com/)
- [日付の計算方法を学ぼう！](https://qiita.com/morizotter/items/195a14d669c50dcb000c#mac%E3%82%BF%E3%83%BC%E3%83%9F%E3%83%8A%E3%83%AB%E3%81%A7%E6%97%A5%E4%BB%98%E3%82%92%E8%A8%88%E7%AE%97%E3%81%97%E3%81%9F%E3%81%84)
- [Fish Shellを使ってコマンドラインで日付を計算する方法](https://www.esri.com/arcgis-blog/products/product/operations/data-management/how-to-calculate-numeric-dates-in-fish-shell/)
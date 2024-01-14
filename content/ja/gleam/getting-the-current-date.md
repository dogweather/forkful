---
title:    "Gleam: 現在の日付を取得する"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

Gleamプログラミング：現在の日付を取得する

##なぜ

日付を取得することは、多くのプログラマーにとって一般的なタスクです。プログラム内で時刻や日付のデータを使用する必要があるため、正確な日付を取得する必要があります。Gleamを使用すると、簡単に現在の日付を取得することができます。

##やり方

日付を取得する方法は、GleamのDateモジュールを使用することです。以下のサンプルコードをご覧ください。

```Gleam
import Date

let today = Date.now()
```

上記のコードでは、現在の日付が `today`変数に格納されます。また、日付のフォーマットを指定することもできます。例えば、以下のコードでは、月-日-年の順で日付を取得します。

```Gleam
let today = Date.to_string(Date.MonthDayYear)(Date.now())
```

これにより、現在の日付が "4-7-2020"のように表示されます。

##深い掘り下げ

さらに詳しく日付を取得するためには、Dateモジュールのドキュメントを参照することができます。また、タイムゾーンの考慮や日付の比較、書式設定など、より高度な操作を行うこともできます。

##関連リンク

- [Gleam公式ドキュメント](https://gleam.run/documentation/)
- [Dateモジュールのドキュメント](https://gleam.run/modules/gleam_stdlib/Date.html)
- [GleamのGitHubリポジトリ](https://github.com/gleam-lang/gleam)
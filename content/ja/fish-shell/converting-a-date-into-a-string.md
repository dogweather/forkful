---
title:    "Fish Shell: 日付を文字列に変換する"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換することには、いくつかの理由があります。日付を文字列に変換することで、より柔軟性のあるデータ形式に変換することができます。また、文字列形式の日付により、日付データを他のアプリケーションやデータベースと簡単に共有することができます。

## 方法

日付を文字列に変換するには、Fish Shellの組み込み機能である`date`コマンドを使用します。以下の例をご覧ください。

```Fish Shell
set today (date +%Y-%m-%d)
echo $today
```

上記のコードでは、現在の日付を指定された形式で文字列として変数`today`に保存し、その値を出力しています。出力結果は`2021-10-30`のようになります。

また、日付を含めたより複雑な文字列に変換することもできます。例えば、以下のようなコードを使用して日付と曜日を含む文字列を作成できます。

```Fish Shell
set today (date "+%Y年%m月%d日 %a")
echo $today
```

出力結果は、`2021年10月30日 土`となり、曜日まで含まれることがわかります。

## 掘り下げる

`date`コマンドの具体的なオプションや書式については、[公式ドキュメント](https://fishshell.com/docs/current/cmds/date.html)を参照してください。さらに、日付や時刻に関するより詳細な情報を得たい場合には、[GNU Core Utilities](https://www.gnu.org/software/coreutils/)や[Date and Time Utilities](https://unixhelp.ed.ac.uk/CGI/man-cgi?date)などのリソースも参考になります。

## 関連情報を見る

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [GNU Core Utilities](https://www.gnu.org/software/coreutils/)
- [Date and Time Utilities](https://unixhelp.ed.ac.uk/CGI/man-cgi?date)
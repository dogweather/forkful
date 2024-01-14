---
title:    "Fish Shell: 「現在の日付の取得」"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ？ 

現在の日付を取得するのには、プログラミングでよく使用されることがあります。例えば、ファイル名やログに日付を追加したい場合や、期限の設定など、様々なシナリオで日付情報が必要になるので、Fish Shellで現在の日付を取得する方法を学ぶことは重要です。 

## 方法 

Fish Shellでは、コマンド「`date`」を使用して簡単に現在の日付を取得することができます。下記のコード例を参考にしてください。 

```Fish Shell
date
```

これにより、コマンドを実行した時点の日付が表示されます。また、より詳細な日付情報が必要な場合は、`date`コマンドに引数を追加することで、任意のフォーマットで日付を取得することもできます。下記の例を参考にしてください。 

```Fish Shell
date "+%Y-%m-%d %H:%M:%S"
```

この場合は、現在の年月日と時分秒の情報が表示されます。 

## 深堀り 

`date`コマンドの引数にどのようなフォーマットを指定するかによって、表示される日付の情報が変わります。例えば、`%Y`は西暦での年、`%m`は月、`%d`は日を表します。また、`%H`は24時間表記の時、`%M`は分、`%S`は秒を表します。他にも様々なフォーマットが存在するので、調べてみると面白いかもしれません。 

## See Also 

- [Fish Shell公式サイト](https://fishshell.com)
- [Fish Shellのドキュメンテーション](https://fishshell.com/docs/current/index.html)
- [dateコマンドのマニュアルページ](https://manpages.debian.org/stretch/date/date.1.en.html)
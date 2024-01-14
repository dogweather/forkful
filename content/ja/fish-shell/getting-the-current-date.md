---
title:                "Fish Shell: 「現在の日付の取得」"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
現在の日付を取得することの重要性を説明する1〜2文。

## 方法
```Fish Shell```でのコーディング例と、サンプル出力を含む```コードブロック```を使用した説明。

### 日付の取得
現在の日付を取得するには、```date```コマンドを使用します。以下のコードをターミナルに入力して実行すると、現在の日付が表示されます。

```
date
```

出力： ```2021年 9月 24日 金曜日 23時33分30秒 JST```

さらに、特定の書式を指定して日付を表示することもできます。例えば、年-月-日の形式で表示する場合は、以下のようにコマンドを変更します。

```
date +"%Y-%m-%d"
```

出力： ```2021-09-24```

詳細な書式指定については、[manページ](https://fishshell.com/docs/current/cmds/date.html)を参照してください。

## 詳細を調べる
日付を取得する方法についてさらに詳しく知りたい場合は、「```date```コマンド」や「```Fish Shell```の日付操作」に関するドキュメントを参照することをおすすめします。

## 参考情報
- [日付を操作するコマンド ```date```の使い方](https://techacademy.jp/magazine/18883)
- [Fish Shell ドキュメンテーション - 日付操作](https://fishshell.com/docs/current/cmds/date.html)
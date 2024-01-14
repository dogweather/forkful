---
title:    "Fish Shell: 「現在の日付を取得する」"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Why
なぜ現在の日付を取得する必要があるのでしょうか？日付を取得することで、タスク管理やファイル命名など、日常のさまざまな作業に便利です。

## How To
まず、Fish Shellを開きます。以下のコマンドを入力し、現在の日付を取得することができます。

```Fish Shell
date
```

これにより、現在の日付が表示されます。もし、特定の形式で日付を取得したい場合は、以下のようにコマンドを入力します。

```Fish Shell
date "+%Y年%m月%d日"
```

これにより、例えば"2020年11月28日"のような形式で日付が表示されます。さらに、曜日や時間を追加したい場合は、以下のようにコマンドを入力します。

```Fish Shell
date "+%Y年%m月%d日 (%A) %H時%M分%S秒"
```

これにより、"2020年11月28日 (土曜日) 12時13分45秒"のような形式で日付が表示されます。詳しい日付のフォーマットは、Fish Shellの公式ドキュメントを参照してください。

## Deep Dive
現在の日付を取得するコマンドの背景には、Unixの基本的なコマンドである"date"が使用されています。このコマンドでは、コンピューター内部で保持されているタイムスタンプを解析し、人間が理解しやすい形式で日付を表示するようにしています。さらに、日付を取得するコマンドは、プログラミング言語やスクリプトで使用することもできます。

## See Also
さらに日付を取得するためのコード例や詳しい情報を知りたい方は、以下のリンクを参考にしてください。

- Official Fish Shell Documentation: https://fishshell.com/docs/current/cmds/date.html
- Tutorialspoint article on getting current date in Fish Shell: https://www.tutorialspoint.com/fish-shell/fish_shell_date_command.htm
- The Unix Timestamp Converter: https://www.unixtimestamp.com/
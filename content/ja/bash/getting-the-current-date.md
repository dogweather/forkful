---
title:    "Bash: 現在の日付を取得する"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## なぜ

現在の日付を取得することに興味を持つ理由は様々です。例えば、ファイル名に日付を含めることで、データをより組織的に管理することができます。また、日付の情報を基に、自動的にタスクを実行するプログラムを作成することができます。

## 方法

現在の日付を取得するには、Bashの `date` コマンドを使用します。以下のように入力することで、今日の日付を表示することができます。

```Bash
date
```

もし、年月日を `YYYYMMDD` の形式で取得したい場合は、 `-I` オプションを使用します。

```Bash
date -I
```

また、特定の形式で日付を取得することもできます。例えば、 `YYYY/MM/DD` の形式で取得するには、 `-I='/'` オプションを付けます。

```Bash
date -I='/'
```

## ディープダイブ

`date` コマンドは、システム時刻を表示するだけではありません。様々なオプションを使用することで、現在の日付や時刻を様々な形式で表示することができます。また、環境変数を使用することで、ロケールやタイムゾーンを変更することもできます。

## See Also（関連情報）

- [Bash Beginner's Guide](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/index.html)
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/Bash-Beginners-Guide.html)
- [bash(1) - Linux man page](https://linux.die.net/man/1/bash)
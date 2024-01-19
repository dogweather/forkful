---
title:                "現在の日付の取得"
html_title:           "Bash: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何となぜ？

現在の日付を取得するとは、組み込みコマンドや関数を用いてコンピュータのシステム時刻から日付情報を抽出することです。これは、プログラムが実行時の時間依存データを作成したり、ユーザが特定のタスクを完了した時刻を追跡するのに役立つため、プログラマーが行います。

## どうやって：

Fish Shellで現在の日付を取得するためのコードは以下の通りです：

```Fish Shell
set current_date (date "+%Y-%m-%d")
echo $current_date
```
上記のコードを実行すると、現在の日付が `YYYY-MM-DD` 形式で表示されます。

## ディープダイブ：

歴史的な組み込みコマンド 'date' はUNIXおよびLinuxオペレーティングシステムの初期から利用可能でした。Fish Shellコマンド `date "+%Y-%m-%d"` で現在の日付を取得します。

代替手段としてPythonやPerlなどの他のスクリプト言語から日付を取得することも可能ですが、Fish Shellはこれらの機能をシェルの一部として提供しています。

Fish Shellの日付コマンドはGNUコアユーティリティの 'date' コマンドを利用しています。具体的にはCライブラリ関数 `time` と関連関数（`localtime`や`strftime`など）を背景で呼び出しています。

## 参考文献：

- [Fish Shell公式ドキュメンテーション](https://fishshell.com/docs/current/index.html)
- [GNU Coreutils: date拡張](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [Linuxコマンド：date](https://man7.org/linux/man-pages/man1/date.1.html)
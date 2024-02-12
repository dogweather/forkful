---
title:                "現在の日付の取得"
aliases:
- ja/fish-shell/getting-the-current-date.md
date:                  2024-02-03T19:09:34.615080-07:00
model:                 gpt-4-0125-preview
simple_title:         "現在の日付の取得"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
プログラミングで現在の日付を取得することは、システムの日付と時刻のデータを取得して操作するための基本的な作業です。スクリプトや自動化タスクでは、タイムスタンプの生成、タスクのスケジューリング、ログの作成に不可欠です。

## 方法：
Fish Shellは、必要に応じて出力の形式を整形する柔軟性を提供する`date`のような外部コマンドを利用して現在の日付を取得します。使い方は以下の通りです：

```fish
# デフォルト形式で現在の日付を表示
echo (date)

# 出力例：Wed 25 Oct 2023 15:42:03 BST
```

日付の形式をカスタマイズするには、フォーマット指定子に続けて`+`オプションを使用できます：

```fish
# YYYY-MM-DD形式で現在の日付を表示
echo (date "+%Y-%m-%d")

# 出力例：2023-10-25
```

タイムスタンプの取り扱いや日付演算のようなもっと複雑なタスクに対しては、スクリプティングの性質上、Fish Shellは`date`のような外部ツールに頼っています。現在のUNIXタイムスタンプを取得する例を以下に示します：

```fish
# 現在のUNIXタイムスタンプを取得
echo (date "+%s")

# 出力例：1666710123
```

そして`date`を使って現在の日付に一日を加えるには：

```fish
# 現在の日付に一日加える
echo (date -d "+1 day" "+%Y-%m-%d")

# 出力例：2023-10-26
```

注：例ではGNU coreutilsで動作する`date`コマンドのオプションを使用しています。macOSなど、デフォルトでBSD dateコマンドを使用する他の環境ではオプションが異なる場合があります。常に`date --help`またはmanページを参照して、ご自身の環境に特有の詳細を確認してください。

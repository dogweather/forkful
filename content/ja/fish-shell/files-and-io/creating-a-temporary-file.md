---
title:                "一時ファイルの作成"
aliases:
- /ja/fish-shell/creating-a-temporary-file/
date:                  2024-01-20T17:40:22.423277-07:00
model:                 gpt-4-1106-preview
simple_title:         "一時ファイルの作成"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
一時ファイルを作成することは、短期的なデータ保存のためです。プログラマーは、データの一時的な処理やテスト、または他のプログラムとのデータ共有のためにこれを行います。

## How to: (方法)
```Fish Shell
# 一時ファイルの作成
set tempfile (mktemp)
echo "これは一時ファイルです" >> $tempfile

# 作成した一時ファイルの内容の確認
cat $tempfile

# 使い終わった一時ファイルの削除
rm $tempfile
```

サンプル出力:
```
これは一時ファイルです
```

## Deep Dive (深い潜水)
一時ファイルは、UNIX系オペレーティングシステムで長い間使われています。`mktemp` コマンドは安全にユニークな一時ファイルを作成できる標準的ツールです。他の方法としては、`tmpfile` システムコールを使うCライブラリ関数もあります。Fish Shell では独自の一時ファイル管理はありませんが、UNIXの標準ツールを利用して容易に実装可能です。

## See Also (関連する情報)
- Fish Shell 公式ドキュメント: https://fishshell.com/docs/current/index.html
- `mktemp` マニュアルページ: https://man7.org/linux/man-pages/man1/mktemp.1.html
- UNIX系一時ファイルの歴史: https://en.wikipedia.org/wiki/Temporary_folder

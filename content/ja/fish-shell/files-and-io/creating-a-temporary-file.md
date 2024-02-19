---
aliases:
- /ja/fish-shell/creating-a-temporary-file/
date: 2024-01-20 17:40:22.423277-07:00
description: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3059\u308B\u3053\
  \u3068\u306F\u3001\u77ED\u671F\u7684\u306A\u30C7\u30FC\u30BF\u4FDD\u5B58\u306E\u305F\
  \u3081\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\
  \u30BF\u306E\u4E00\u6642\u7684\u306A\u51E6\u7406\u3084\u30C6\u30B9\u30C8\u3001\u307E\
  \u305F\u306F\u4ED6\u306E\u30D7\u30ED\u30B0\u30E9\u30E0\u3068\u306E\u30C7\u30FC\u30BF\
  \u5171\u6709\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.327395
model: gpt-4-1106-preview
summary: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3059\u308B\u3053\
  \u3068\u306F\u3001\u77ED\u671F\u7684\u306A\u30C7\u30FC\u30BF\u4FDD\u5B58\u306E\u305F\
  \u3081\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\
  \u30BF\u306E\u4E00\u6642\u7684\u306A\u51E6\u7406\u3084\u30C6\u30B9\u30C8\u3001\u307E\
  \u305F\u306F\u4ED6\u306E\u30D7\u30ED\u30B0\u30E9\u30E0\u3068\u306E\u30C7\u30FC\u30BF\
  \u5171\u6709\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
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

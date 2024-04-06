---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:34.615080-07:00
description: "\u65B9\u6CD5\uFF1A Fish Shell\u306F\u3001\u5FC5\u8981\u306B\u5FDC\u3058\
  \u3066\u51FA\u529B\u306E\u5F62\u5F0F\u3092\u6574\u5F62\u3059\u308B\u67D4\u8EDF\u6027\
  \u3092\u63D0\u4F9B\u3059\u308B`date`\u306E\u3088\u3046\u306A\u5916\u90E8\u30B3\u30DE\
  \u30F3\u30C9\u3092\u5229\u7528\u3057\u3066\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\
  \u5F97\u3057\u307E\u3059\u3002\u4F7F\u3044\u65B9\u306F\u4EE5\u4E0B\u306E\u901A\u308A\
  \u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:38:42.235842-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Fish Shell\u306F\u3001\u5FC5\u8981\u306B\u5FDC\u3058\u3066\
  \u51FA\u529B\u306E\u5F62\u5F0F\u3092\u6574\u5F62\u3059\u308B\u67D4\u8EDF\u6027\u3092\
  \u63D0\u4F9B\u3059\u308B`date`\u306E\u3088\u3046\u306A\u5916\u90E8\u30B3\u30DE\u30F3\
  \u30C9\u3092\u5229\u7528\u3057\u3066\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\
  \u3057\u307E\u3059\u3002\u4F7F\u3044\u65B9\u306F\u4EE5\u4E0B\u306E\u901A\u308A\u3067\
  \u3059\uFF1A."
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

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

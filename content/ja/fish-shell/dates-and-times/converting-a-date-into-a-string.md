---
date: 2024-01-20 17:36:26.486971-07:00
description: "How to: (\u65B9\u6CD5\uFF1A) \u904E\u53BB\u3001Unix/Linux \u7CFB\u306E\
  \u30B7\u30A7\u30EB\u3067\u306F\u65E5\u4ED8\u3068\u6642\u523B\u306E\u64CD\u4F5C\u306B\
  \ `date` \u30B3\u30DE\u30F3\u30C9\u304C\u4F7F\u308F\u308C\u3066\u304D\u307E\u3057\
  \u305F\u3002Fish Shell\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.616667-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5\uFF1A) \u904E\u53BB\u3001Unix/Linux \u7CFB\u306E\u30B7\u30A7\
  \u30EB\u3067\u306F\u65E5\u4ED8\u3068\u6642\u523B\u306E\u64CD\u4F5C\u306B `date`\
  \ \u30B3\u30DE\u30F3\u30C9\u304C\u4F7F\u308F\u308C\u3066\u304D\u307E\u3057\u305F\
  \u3002Fish Shell \u3067\u3082\u3053\u306E\u30B3\u30DE\u30F3\u30C9\u3092\u5229\u7528\
  \u3057\u3001\u65E5\u4ED8\u306E\u6587\u5B57\u5217\u5909\u63DB\u304C\u53EF\u80FD\u3067\
  \u3059\u3002\u4ED6\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u306B\
  \u3082\u540C\u69D8\u306E\u6A5F\u80FD\u304C\u3042\u308A\u307E\u3059\u304C\u3001\u30B7\
  \u30A7\u30EB\u30B9\u30AF\u30EA\u30D7\u30C8\u3067\u306F\u76F4\u63A5\u7684\u306A\u65B9\
  \u6CD5\u3067\u6271\u3048\u308B\u306E\u304C\u9B45\u529B\u3067\u3059\u3002Fish \u3067\
  \u306F\u65E5\u4ED8\u30C7\u30FC\u30BF\u3092 +\u30AA\u30D7\u30B7\u30E7\u30F3\u3068\
  \u7D44\u307F\u5408\u308F\u305B\u3066\u3001\u591A\u69D8\u306A\u30D5\u30A9\u30FC\u30DE\
  \u30C3\u30C8\u306B\u5BFE\u5FDC\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\
  \u3002"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

## How to: (方法：)
```Fish Shell
# 現在の日付と時刻を取得し、独自の形式で出力
set current_date (date)
echo $current_date

# 日付を YYYY-MM-DD 形式で出力
set formatted_date (date "+%Y-%m-%d")
echo $formatted_date
```

Sample Output:
```
Sun Mar 14 15:02:56 JST 2021
2021-03-14
```

## Deep Dive (深掘り)
過去、Unix/Linux 系のシェルでは日付と時刻の操作に `date` コマンドが使われてきました。Fish Shell でもこのコマンドを利用し、日付の文字列変換が可能です。他のプログラミング言語にも同様の機能がありますが、シェルスクリプトでは直接的な方法で扱えるのが魅力です。Fish では日付データを +オプションと組み合わせて、多様なフォーマットに対応することができます。

例えば：
- `%Y`: 年を4桁で表示。
- `%m`: 月を2桁で表示。
- `%d`: 日を2桁で表示。

これにより、ログファイルのタイムスタンプや、プログラムの出力を整形する際、柔軟性と読みやすさを提供します。

## See Also (関連項目)
- Fish Shell 公式ドキュメント: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- `date` コマンドのマニュアルページ: [https://man7.org/linux/man-pages/man1/date.1.html](https://man7.org/linux/man-pages/man1/date.1.html)
- POSIX `strftime` フォーマット指定子: [https://pubs.opengroup.org/onlinepubs/9699919799/functions/strftime.html](https://pubs.opengroup.org/onlinepubs/9699919799/functions/strftime.html)

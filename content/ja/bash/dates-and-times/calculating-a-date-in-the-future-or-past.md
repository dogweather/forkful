---
date: 2024-01-20 17:31:03.115471-07:00
description: "How to: (\u3084\u308A\u65B9) \u904E\u53BB\u3084\u672A\u6765\u306E\u65E5\
  \u4ED8\u3092\u8A08\u7B97\u3059\u308B\u3068\u304D\u3001`date` \u30B3\u30DE\u30F3\u30C9\
  \u306F\u975E\u5E38\u306B\u6709\u7528\u3067\u3059\u3002Unix\u30671970\u5E74\u4EE3\
  \u306B\u958B\u767A\u3055\u308C\u305F\u3053\u306E\u30B3\u30DE\u30F3\u30C9\u306F\u3001\
  Linux\u30B7\u30B9\u30C6\u30E0\u7BA1\u7406\u3084\u30B9\u30AF\u30EA\u30D7\u30C8\u3067\
  \u5E83\u304F\u4F7F\u308F\u308C\u3066\u3044\u307E\u3059\u3002GNU `date` \u306F\u975E\
  \u5E38\u306B\u67D4\u8EDF\u3067\u3001\u8907\u6570\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\
  \u30C8\u3084\u30AA\u30D7\u30B7\u30E7\u30F3\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\
  \u3044\u307E\u3059\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:41.905974-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u904E\u53BB\u3084\u672A\u6765\u306E\u65E5\u4ED8\u3092\
  \u8A08\u7B97\u3059\u308B\u3068\u304D\u3001`date` \u30B3\u30DE\u30F3\u30C9\u306F\u975E\
  \u5E38\u306B\u6709\u7528\u3067\u3059\u3002Unix\u30671970\u5E74\u4EE3\u306B\u958B\
  \u767A\u3055\u308C\u305F\u3053\u306E\u30B3\u30DE\u30F3\u30C9\u306F\u3001Linux\u30B7\
  \u30B9\u30C6\u30E0\u7BA1\u7406\u3084\u30B9\u30AF\u30EA\u30D7\u30C8\u3067\u5E83\u304F\
  \u4F7F\u308F\u308C\u3066\u3044\u307E\u3059\u3002GNU `date` \u306F\u975E\u5E38\u306B\
  \u67D4\u8EDF\u3067\u3001\u8907\u6570\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3084\
  \u30AA\u30D7\u30B7\u30E7\u30F3\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\
  \u3059\u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
weight: 26
---

## How to: (やり方)
```Bash
# 現在の日付から10日後の日付を計算
date "+%Y-%m-%d" -d "10 days"
# 出力例: 2023-04-21(実行日によって異なる)

# 現在の日付から2週間前の日付を計算
date "+%Y-%m-%d" -d "2 weeks ago"
# 出力例: 2023-04-03(実行日によって異なる)
```

## Deep Dive (掘り下げ)
過去や未来の日付を計算するとき、`date` コマンドは非常に有用です。Unixで1970年代に開発されたこのコマンドは、Linuxシステム管理やスクリプトで広く使われています。GNU `date` は非常に柔軟で、複数のフォーマットやオプションをサポートしています。

他の選択肢もあります。例えば `GNU date`、`Python` の `datetime` ライブラリ、あるいは `Perl` とその `Time::Piece` モジュールです。各々特定のシナリオや好みに応じて選ぶことができます。

実装の詳細では、タイムゾーンの扱いや、うるう秒などの特例を考慮する必要がある場合もあります。また、日付の計算は、入力としてGregorian暦を用いることが多いですが、アプリケーションのニーズに応じて他の暦を使うこともあります。

## See Also (関連情報)
- [GNU Coreutils – Date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html): GNU `date` コマンドの公式ドキュメント。
- [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/): シェルスクリプトに関する詳細な情報提供。
- [Python Documentation for datetime](https://docs.python.org/3/library/datetime.html): `datetime` ライブラリの詳細。
- [Perl Time::Piece](https://metacpan.org/pod/Time::Piece): Perlの `Time::Piece` モジュールのドキュメント。

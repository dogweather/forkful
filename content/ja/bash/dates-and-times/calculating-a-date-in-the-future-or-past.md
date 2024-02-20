---
date: 2024-01-20 17:31:03.115471-07:00
description: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\
  \u7B97\u3059\u308B\u3053\u3068\u3068\u306F\u3001\u3042\u308B\u65E5\u4ED8\u304B\u3089\
  \u7279\u5B9A\u306E\u65E5\u6570\u3092\u52A0\u7B97\u3057\u305F\u308A\u6E1B\u7B97\u3057\
  \u305F\u308A\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u671F\u9650\u3092\u8A2D\u5B9A\u3057\u305F\u308A\u3001\u30A4\u30D9\
  \u30F3\u30C8\u30B9\u30B1\u30B8\u30E5\u30FC\u30EB\u3092\u751F\u6210\u3059\u308B\u305F\
  \u3081\u306B\u3053\u306E\u8A08\u7B97\u3092\u3088\u304F\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.517942
model: gpt-4-1106-preview
summary: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\
  \u7B97\u3059\u308B\u3053\u3068\u3068\u306F\u3001\u3042\u308B\u65E5\u4ED8\u304B\u3089\
  \u7279\u5B9A\u306E\u65E5\u6570\u3092\u52A0\u7B97\u3057\u305F\u308A\u6E1B\u7B97\u3057\
  \u305F\u308A\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u671F\u9650\u3092\u8A2D\u5B9A\u3057\u305F\u308A\u3001\u30A4\u30D9\
  \u30F3\u30C8\u30B9\u30B1\u30B8\u30E5\u30FC\u30EB\u3092\u751F\u6210\u3059\u308B\u305F\
  \u3081\u306B\u3053\u306E\u8A08\u7B97\u3092\u3088\u304F\u884C\u3044\u307E\u3059\u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
将来または過去の日付を計算することとは、ある日付から特定の日数を加算したり減算したりすることです。プログラマーは、期限を設定したり、イベントスケジュールを生成するためにこの計算をよく行います。

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

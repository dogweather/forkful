---
aliases:
- /ja/fish-shell/comparing-two-dates/
date: 2024-01-20 17:33:00.479401-07:00
description: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B\u3063\u3066\uFF1F\u30D7\u30ED\
  \u30B0\u30E9\u30E0\u306B\u304A\u3044\u3066\u30012\u3064\u306E\u65E5\u4ED8\u304C\u3069\
  \u3046\u9055\u3046\u306E\u304B\u78BA\u8A8D\u3059\u308B\u624B\u7D9A\u304D\u3067\u3059\
  \u3002\u306A\u305C\u6BD4\u8F03\u3059\u308B\u306E\uFF1F\u65E5\u4ED8\u306E\u5DEE\u3092\
  \u8A08\u7B97\u3059\u308B\u305F\u3081\u3001\u671F\u9650\u3092\u30C1\u30A7\u30C3\u30AF\
  \u3059\u308B\u305F\u3081\u3001\u307E\u305F\u306F\u30A4\u30D9\u30F3\u30C8\u306E\u9806\
  \u5E8F\u3092\u6C7A\u3081\u308B\u305F\u3081\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.321131
model: gpt-4-1106-preview
summary: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B\u3063\u3066\uFF1F\u30D7\u30ED\
  \u30B0\u30E9\u30E0\u306B\u304A\u3044\u3066\u30012\u3064\u306E\u65E5\u4ED8\u304C\u3069\
  \u3046\u9055\u3046\u306E\u304B\u78BA\u8A8D\u3059\u308B\u624B\u7D9A\u304D\u3067\u3059\
  \u3002\u306A\u305C\u6BD4\u8F03\u3059\u308B\u306E\uFF1F\u65E5\u4ED8\u306E\u5DEE\u3092\
  \u8A08\u7B97\u3059\u308B\u305F\u3081\u3001\u671F\u9650\u3092\u30C1\u30A7\u30C3\u30AF\
  \u3059\u308B\u305F\u3081\u3001\u307E\u305F\u306F\u30A4\u30D9\u30F3\u30C8\u306E\u9806\
  \u5E8F\u3092\u6C7A\u3081\u308B\u305F\u3081\u3067\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付を比較するって？プログラムにおいて、2つの日付がどう違うのか確認する手続きです。なぜ比較するの？日付の差を計算するため、期限をチェックするため、またはイベントの順序を決めるためです。

## How to (方法)
Fish Shellで日付を比較する基本です。以下、コードを確認してください。

```Fish Shell
# 日付を比較するサンプルコード
set date1 (date -d '2023-04-01' +%s)
set date2 (date -d '2023-04-15' +%s)

if test $date1 -lt $date2
    echo "date1 is earlier than date2"
else if test $date1 -eq $date2
    echo "date1 is the same as date2"
else
    echo "date1 is later than date2"
end
```

出力例:
```
date1 is earlier than date2
```

## Deep Dive (深堀り)
歴史的背景：UNIX時代から、日付操作はシステム管理やスクリプティングの基本的要素でした。Fish Shellの日付比較機能は、これを使いやすくするための洗練された方法を提供します。

代わりの方法：`date`コマンドの他に、`chron`ライブラリのような外部ツールを使うこともできます。ただし、Fish Shell自身の機能で十分であることが多いです。

実装の詳細：上記のコードでは、`date`コマンドを使ってUNIXタイムスタンプ（エポック秒）に日付を変換し、これを比較しています。UNIXタイムスタンプは1970年1月1日からの秒数です。比較には、Fishの組込み関数`test`を使用します。

## See Also (関連情報)
- [Fish Shell Documentation (英語)](https://fishshell.com/docs/current/index.html)
- [GNU Coreutils - Date (英語)](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [UNIX時間について (日本語)](http://e-words.jp/w/UNIX%E6%99%82%E9%96%93.html)

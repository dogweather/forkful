---
title:                "正規表現の使用"
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
正規表現って何？プログラマが使う理由は？

正規表現は、文字列を検索・置換するための強力なパターンマッチング言語だ。コードを簡単・効率的にし、複雑な文字列処理タスクを容易にこなせるようにするためにプログラマはこれを使う。

## How to:
Rubyでのコード例と出力サンプル。

```Ruby
text = "今日は2023年3月15日です。"
date_pattern = /\d{4}年\d{1,2}月\d{1,2}日/

# パターンマッチング
if text.match(date_pattern)
  puts "日付が見つかりました: #{$&}"
else
  puts "日付は見つかりませんでした。"
end

# 出力: 日付が見つかりました: 2023年3月15日
```

## Deep Dive
正規表現の深堀り：歴史的背景、代替手段、実装の詳細

正規表現は1960年代に発明され、テキスト処理の豊かな歴史がある。Perl言語で人気を博し、その後多くのプログラミング言語で採用された。Rubyでは、正規表現は組み込みクラス`Regexp`として使われ、文字列操作がシームレスに実現される。代替手段としては、文字列の`split`, `index`, `include?`のようなメソッドがあるが、これらは単純なタスク用で、複雑なパターン検出には不向きだ。正規表現はNFA(非決定性有限オートマトン)に基づいて実装されており、多くのケースで効率的だが、適切に使わなければパフォーマンスに影響を与える。

## See Also
関連リンク

- Rubyの公式ドキュメントにおけるRegexpクラス: [Ruby-Doc Regexp](https://ruby-doc.org/core/Regexp.html)
- さらに学ぶためのRegExr: [RegExr](https://regexr.com/)

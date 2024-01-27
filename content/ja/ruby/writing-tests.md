---
title:                "テストの作成"
date:                  2024-01-19
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)
テストコードを書くことは、プログラムが正しく動作するか検証する手法です。バグの早期発見、機能の維持、リファクタリングの安全性向上のために行います。

## How to: (やり方)
```Ruby
def sum(a, b)
  a + b
end

# テストケースの記述
require 'minitest/autorun'

class TestSum < Minitest::Test
  def test_sum
    assert_equal 5, sum(2, 3)
  end
end
```
実行すると、以下のような出力が得られます（成功時）。
```
Run options: --seed 59714

# Running:

.

Finished in 0.001038s, 963.3914 runs/s, 963.3914 assertions/s.

1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

## Deep Dive (深掘り)
テストはXP(eXtreme Programming)という開発手法が広めました。RSpecやCucumberといった代替ツールもあります。`minitest`はRubyの標準ライブラリであり、軽量で読みやすいテストが書けるとされています。

## See Also (関連情報)
- [RSpecホームページ](https://rspec.info/)
- [Cucumber公式サイト](https://cucumber.io/)

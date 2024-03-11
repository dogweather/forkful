---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:07.783325-07:00
description: "Ruby\u3067\u306E\u30C6\u30B9\u30C8\u306F\u3001\u3055\u307E\u3056\u307E\
  \u306A\u6761\u4EF6\u4E0B\u3067\u30B3\u30FC\u30C9\u304C\u671F\u5F85\u901A\u308A\u306B\
  \u52D5\u4F5C\u3059\u308B\u304B\u3092\u691C\u8A3C\u3059\u308B\u3053\u3068\u306B\u3064\
  \u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C6\u30B9\
  \u30C8\u3092\u66F8\u3044\u3066\u3001\u6B63\u78BA\u6027\u3092\u78BA\u4FDD\u3057\u3001\
  \u30EA\u30B0\u30EC\u30C3\u30B7\u30E7\u30F3\u3092\u9632\u304E\u3001\u30EA\u30D5\u30A1\
  \u30AF\u30BF\u30EA\u30F3\u30B0\u3092\u5BB9\u6613\u306B\u3059\u308B\u3053\u3068\u3092\
  \u76EE\u6307\u3057\u3001\u5805\u7262\u3067\u4FDD\u5B88\u53EF\u80FD\u306A\u30A2\u30D7\
  \u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3092\u76EE\u6307\u3057\u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:16.409935-06:00'
model: gpt-4-0125-preview
summary: "Ruby\u3067\u306E\u30C6\u30B9\u30C8\u306F\u3001\u3055\u307E\u3056\u307E\u306A\
  \u6761\u4EF6\u4E0B\u3067\u30B3\u30FC\u30C9\u304C\u671F\u5F85\u901A\u308A\u306B\u52D5\
  \u4F5C\u3059\u308B\u304B\u3092\u691C\u8A3C\u3059\u308B\u3053\u3068\u306B\u3064\u3044\
  \u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C6\u30B9\u30C8\
  \u3092\u66F8\u3044\u3066\u3001\u6B63\u78BA\u6027\u3092\u78BA\u4FDD\u3057\u3001\u30EA\
  \u30B0\u30EC\u30C3\u30B7\u30E7\u30F3\u3092\u9632\u304E\u3001\u30EA\u30D5\u30A1\u30AF\
  \u30BF\u30EA\u30F3\u30B0\u3092\u5BB9\u6613\u306B\u3059\u308B\u3053\u3068\u3092\u76EE\
  \u6307\u3057\u3001\u5805\u7262\u3067\u4FDD\u5B88\u53EF\u80FD\u306A\u30A2\u30D7\u30EA\
  \u30B1\u30FC\u30B7\u30E7\u30F3\u3092\u76EE\u6307\u3057\u307E\u3059\u3002"
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## 何となぜ？
Rubyでのテストは、さまざまな条件下でコードが期待通りに動作するかを検証することについてです。プログラマーはテストを書いて、正確性を確保し、リグレッションを防ぎ、リファクタリングを容易にすることを目指し、堅牢で保守可能なアプリケーションを目指します。

## どのようにして：
Rubyには`Test::Unit`と呼ばれる組み込みライブラリがあり、ユニットテストを書くために、テストの実践を直接的な構造の中にカプセル化します。しかし、Rubyコミュニティは、その高度な表現力と柔軟性のため、RSpecやMinitestといったサードパーティのライブラリを好むことが多いです。

### `Test::Unit`を使用する:
```ruby
require 'test/unit'

class CalculatorTest < Test::Unit::TestCase
  def test_addition
    result = 2 + 2
    assert_equal 4, result
  end
end
```
ターミナルからテストファイルを実行して、テストの成功または失敗を示す出力を得るべきです：
```
Loaded suite test_calculator
Started
.
Finished in 0.001288 seconds.
1 tests, 1 assertions, 0 failures, 0 errors, 0 pendings, 0 omissions, 0 notifications
100% passed
```

### RSpecを使用する:
RSpecはRubyのための人気のあるBDD（行動駆動開発）フレームワークです。`gem install rspec`でgemをインストールし、`rspec --init`でプロジェクトに初期化します。

```ruby
# calculator_spec.rb
require_relative '../calculator'

describe Calculator do
  it 'correctly adds two numbers' do
    expect(Calculator.add(2, 2)).to eq(4)
  end
end
```
`rspec`コマンドでテストを実行します。例の出力：
```
.

Finished in 0.002 seconds (files took 0.1 seconds to load)
1 example, 0 failures
```

### Minitestを使用する:
MinitestはTDD、BDD、モック、およびベンチマークをサポートするテスト施設の完全なスイートを提供します。`gem install minitest`でインストールし、次のように使用します：

```ruby
# test_calculator.rb
require 'minitest/autorun'
require_relative '../calculator'

class CalculatorTest < Minitest::Test
  def test_addition
    assert_equal 4, Calculator.add(2, 2)
  end
end
```

テストファイルを直接実行するか、minitestのために設定された`rake`タスクを通して実行します。サンプル出力：
```
Run options: --seed 33407

# Running:

.

Finished in 0.001027s, 974.5922 runs/s, 974.5922 assertions/s.
1 runs, 1 assertions, 0 failures, 0 errors, 0 skips
```

これらのライブラリを使用してRubyプロジェクトでテストを実装することで、より信頼性が高く、保守が容易なコードベースを目指すベストプラクティスに従うことになります。

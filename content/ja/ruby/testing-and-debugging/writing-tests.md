---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:07.783325-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A \u30BF\u30FC\u30DF\u30CA\
  \u30EB\u304B\u3089\u30C6\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u5B9F\u884C\u3057\
  \u3066\u3001\u30C6\u30B9\u30C8\u306E\u6210\u529F\u307E\u305F\u306F\u5931\u6557\u3092\
  \u793A\u3059\u51FA\u529B\u3092\u5F97\u308B\u3079\u304D\u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:38:42.347369-06:00'
model: gpt-4-0125-preview
summary: ":Unit`\u3068\u547C\u3070\u308C\u308B\u7D44\u307F\u8FBC\u307F\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u304C\u3042\u308A\u3001\u30E6\u30CB\u30C3\u30C8\u30C6\u30B9\u30C8\
  \u3092\u66F8\u304F\u305F\u3081\u306B\u3001\u30C6\u30B9\u30C8\u306E\u5B9F\u8DF5\u3092\
  \u76F4\u63A5\u7684\u306A\u69CB\u9020\u306E\u4E2D\u306B\u30AB\u30D7\u30BB\u30EB\u5316\
  \u3057\u307E\u3059\u3002\u3057\u304B\u3057\u3001Ruby\u30B3\u30DF\u30E5\u30CB\u30C6\
  \u30A3\u306F\u3001\u305D\u306E\u9AD8\u5EA6\u306A\u8868\u73FE\u529B\u3068\u67D4\u8EDF\
  \u6027\u306E\u305F\u3081\u3001RSpec\u3084Minitest\u3068\u3044\u3063\u305F\u30B5\u30FC\
  \u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u597D\u3080\
  \u3053\u3068\u304C\u591A\u3044\u3067\u3059\u3002"
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

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

---
title:                "テストの作成"
aliases:
- /ja/ruby/writing-tests/
date:                  2024-02-03T19:32:07.783325-07:00
model:                 gpt-4-0125-preview
simple_title:         "テストの作成"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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

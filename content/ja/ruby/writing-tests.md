---
title:    "Ruby: テストの書き方"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/writing-tests.md"
---

{{< edit_this_page >}}

# なぜテストを書くのか

プログラミングにおいて、テストを書くことは非常に重要です。テストはコードの品質を確保し、バグを見つけることができる貴重なツールです。正しいテストを書くことで、より信頼性の高いソフトウェアを作ることができます。

# 方法

テストを書くには、Rubyの標準ライブラリに含まれる「Test::Unit」を使用することができます。以下の例を参考にしてください。

```Ruby
require 'test/unit'

class CalculatorTest < Test::Unit::TestCase       # テストケースを定義
  def setup                                        # テスト実行前に必ず実行されるメソッド
    @calculator = Calculator.new                   # テスト対象のオブジェクトを作成
  end

  def test_add                                     # テストメソッドを定義
    assert_equal(5, @calculator.add(2, 3))          # 期待値と実際の値を比較
  end

  def teardown                                     # テスト実行後に必ず実行されるメソッド
    # テストケースごとにクリーンアップが必要な場合はここに記述する
  end
end
```

上記のように、テストケースを定義し、テストメソッド内で今回のテストの「期待値」と「実際の値」を比較することで、テストを行うことができます。テストが失敗した場合はエラーメッセージが表示され、どこが間違っているかが分かりやすくなります。

# 詳細を掘り下げる

テストを書く際には、カバレッジという概念にも注意する必要があります。カバレッジとは、テストでカバーされていないコードの割合を表す指標です。できるだけカバレッジを高くし、隠れたバグを見つけることが重要です。

また、テストを書く際には「AAAパターン」や「テスト駆動開発(TDD)」といったプラクティスも参考にすると良いでしょう。これらの方法を使用することで、より効率的にテストを行うことができます。

# 併せて読みたい
- [RubyでTDDを実践するためのTips](https://qiita.com/jnchito/items/b96193decc64f7fa00da)
- [テスト駆動開発とは？基本的な考え方やメリット・デメリットを理解しよう](https://codezine.jp/article/detail/9698)
- [カバレッジを高めるためのテスト方法とテストコードの網羅性](https://dev.classmethod.jp/articles/raise-test-coverage/)
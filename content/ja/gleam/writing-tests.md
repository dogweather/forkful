---
title:                "Gleam: テストの書き方"
simple_title:         "テストの書き方"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## なぜテストを書く必要があるのか

テストを書くことは、プログラムのバグを見つけやすくし、安心してコードを変更することができるようにします。また、より効率的なコードを書くためにも役立ちます。

## テストの書き方

テストを書くためには、まず最初に`gleam test`コマンドを使ってテストファイルを作成します。その後、`assert`を使用してテストケースを作成し、テストしたい関数を呼び出します。最後に、期待される結果を`=`で指定します。

例えば、以下のように`add`関数をテストすることができます。

```Gleam
test "adds two numbers" {
  assert add(2, 3) = 5 
}
```

## テストの詳細

テストをより複雑にすることで、さまざまな状況をテストすることができます。例えば、異なる入力値に対する結果をテストすることもできます。また、`gleam check`コマンドを使用してコードカバレッジをチェックすることもできます。

テストを書くことで、より信頼性の高いコードを作成することができます。また、継続的なテストを行うことで、コードの品質を保つことができます。

## See Also

- [Gleamの公式ドキュメント](https://gleam.run/documentation/)
- [GleamのGithubリポジトリ](https://github.com/gleam-lang/gleam)
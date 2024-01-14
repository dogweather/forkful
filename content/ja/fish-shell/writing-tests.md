---
title:    "Fish Shell: テストを書く"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## なぜ
テストを書くことのメリットは何でしょうか？プロジェクトで開発しているコードを常に最新の状態に保つことや、バグを発見しやすくすることができます。

## 方法
テストを書くためには、Fish Shellの`begin`と`end`ブロックを使用します。例えば、以下のようにコードを記述します。

```Fish Shell
begin
# ここにテストしたいコードを記述します。
end
```

また、`assert`コマンドを使用することで、テストしたいコードの期待する出力を指定することもできます。例えば、以下のようにコードを記述します。

```Fish Shell
begin
assert "command" "expected_output"
end
```

テストを実行するには、ターミナルで`fish test.fish`と入力します。このコマンドにより、テストが実行され、正しく動作するかどうかが確認されます。

## ディープダイブ
テストを書く際には、様々なテストフレームワークやライブラリを使用することもできます。また、テストの種類によっても異なる書き方があります。全てのテストをカバーすることは難しいですが、プロジェクトのニーズに合わせて最適なテストを書くことが重要です。

## 関連情報を見る
- Fish Shellの公式ドキュメント: https://fishshell.com/docs/current/
- テストの書き方についてのベストプラクティス: https://docs.python-guide.org/writing/tests/
- Fish Shellのテストフレームワーク「bats」のドキュメント: https://github.com/bats-core/bats-core#documentation

## 参考リンク
- https://betterprogramming.pub/writing-tests-in-fish-shell-f637fc09729d
- https://medium.com/@samuelfranklyn/testing-shell-scripts-fish-bash-and-zsh-with-bats-693efc3b9716
- https://fish-shell.readthedocs.io/en/latest/tutorial.html#tests
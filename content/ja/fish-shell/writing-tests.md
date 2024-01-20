---
title:                "記述確認"
html_title:           "Fish Shell: 記述確認"
simple_title:         "記述確認"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

「## 何ですか？・どうしてですか？」
テストを書くことは、コードを確実に動作させるために、プログラマーが行う作業です。テストを書くことによって、コードのバグを発見しやすくしたり、コードの品質を向上させることができます。

「## 作り方：」
あなたのFish Shellプログラムにテストを書く方法を学びましょう。以下のコードブロックを見て、実際にコードを書いてみましょう。

```Fish Shell ...

# test_example.fishというファイルを作成して、以下のコマンドを入力して保存します。
function add
    math "$argv[1] + $argv[2]"
end

# テストを書くための関数を作成します。
function test_add
    if [ (add 2 3) -eq 5 ]
        echo "Add function works correctly"
    else
        echo "Add function is not working properly"
    end
end

# テストを実行して結果を確認します。
test_add

```

以下のような出力が得られれば、テストは成功しています。

```Fish Shell ...
Add function works correctly
```

「## 深堀り：」
テストを書くことの歴史的な背景や、代替方法、実装の詳細について詳しく知りたい方は、以下のリンクを参考にしてください。

「## 関連情報：」
テストの具体的な書き方や、テストを書くことの重要性についてより詳しく知りたい方は、以下のリンクを参考にしてください。

- [Fish Shell公式ドキュメント](https://fishshell.com/docs/current/index.html)
- [テスティングに関するベストプラクティス](https://bit.ly/2HDDN5x)
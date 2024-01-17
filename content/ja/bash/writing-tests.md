---
title:                "テストの作成"
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/writing-tests.md"
---

{{< edit_this_page >}}

## 何をして、なぜそれをするの？ 
テストを書くとは、プログラマーが自分が書いたコードを自動的にテストすることです。これにより、バグを早期に発見し、品質の高いソフトウェアを作ることができます。

## やり方： 
```Bash
# テストを書く基本的な手順
# テストを実行するためには、実際のコードの前に「test」というキーワードを使います。
# テスト関数には、入力値と期待される出力値を指定します。

# テストを書く例
# この例では、スクリプトをあなたのディレクトリ内のファイルを数えるものだとします。
# count_files関数を書きますが、実際の仕事をしていて、正しく機能するかどうかを確認するためのテストをしなければなりません。

count_files() {
    num_files=$(ls | wc -l)
    echo "Number of files in directory: $num_files"
}

test_count_files() {
    echo "Testing count_files function..."
    if [ "$(count_files)" == "$(ls -1 | wc -l)" ]
    then
        echo "PASS!"
    else
        echo "FAIL!"
    fi
}

# テストを実行する
# test_count_files関数を実行して、出力結果を見ます。
test_count_files
```

## より詳しく：
テストを書くことは、品質保証プロセスの重要な部分であり、プログラマーにとって非常に重要です。テストを書くことで、バグを早期に発見することができ、変更を加えた時にも正しく機能するかどうかを確認することができます。代替手段として、手動でテストを実行することもできますが、効率的ではありません。テストを自動化することで、より迅速かつ正確にプログラムの品質を確保することができます。

## 参考: 
- テストの書き方：https://www.tutorialspoint.com/unix/unix-writing-tests.htm
- テストの実行：https://www.tutorialspoint.com/unix/unix-executing-tests.htm
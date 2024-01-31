---
title:                "エラー処理"
date:                  2024-01-26T00:50:03.931891-07:00
model:                 gpt-4-1106-preview
simple_title:         "エラー処理"

category:             "Bash"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/handling-errors.md"
---

{{< edit_this_page >}}

## 何となぜ？

Bashスクリプティングにおけるエラー処理は、何がうまくいかない可能性があるかを予測して、スムーズに対処することについてです。なぜかと言うと、スクリプトを堅牢にし、予想通りに動作しないときにユーザーが困惑するのを防ぐためです。

## 方法：

```Bash
#!/bin/bash

# 標準エラー出力をファイルにリダイレクトする
grep "something" file.txt 2> errors.log

# 終了ステータスを利用したエラー処理
if ! grep "something" file.txt; then
    echo "おっと、'something' を検索中に何か問題が発生しました。"
    exit 1
fi

# エラー終了時に cleanup を行うために trap を使用する
cleanup() {
  echo "一時ファイルをクリーンアップしています..."
  rm temp_*
}

trap cleanup ERR

# 故意のエラー：ファイルが存在しない
cat temp_file.txt
```

エラー発生時のサンプル出力：

```
一時ファイルをクリーンアップしています...
cat: temp_file.txt: そのようなファイルやディレクトリはありません
```

## 詳細

Bashスクリプティングにおけるエラー処理は、Unixシェルの起源に遡り、堅牢で信頼性の高いスクリプトは（今でも）システム管理や自動化に不可欠でした。従来、Bashでのエラーはコマンドの終了ステータスをチェックすることで処理され、慣例により成功は0を、失敗は非ゼロ値を返すようになっています。

Bashは`trap`コマンドを組み込みで導入し、ユーザーが様々なシグナルやスクリプト終了時に実行するコマンドを指定できるようにしました。これはクリーンアップタスクや最後の手段としてのエラー処理メカニズムに有用です。

また、エラー時のBashの動作を変更する`set`コマンドもあります。たとえば、`set -e`を使うと、非ゼロステータスで終了するコマンドがあるとスクリプトが直ちに終了するようになり、連鎖するエラーを避けて迅速に失敗する方法です。

Bash組み込みのエラー処理の代替手段には、ファイルの存在を明示的にチェックする、コマンド置換を使用する、さらには独自の関数を書いてエラーをよりきめ細かく処理する方法があります。

厳密なエラー処理は小さなスクリプトには過剰と感じることもありますが、デバッグに費やす時間を節約し、あなたとユーザーの両方にとって意図しない挙動を防ぐ実践です。

## 参照

- Bashマニュアルのシェルパラメータについて：https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
- 上級Bashスクリプティングガイドのエラー処理セクション：https://www.tldp.org/LDP/abs/html/exit-status.html
- `trap`についての詳細ガイド：https://mywiki.wooledge.org/SignalTrap

覚えておいてください、スクリプティングは芸術形式です。その中での小さな失敗や障害をどのように扱うかが、あなたの作品をより強固にします。ハッピースクリプティング！

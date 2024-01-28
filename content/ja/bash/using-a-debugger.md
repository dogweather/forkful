---
title:                "デバッガーの使い方"
date:                  2024-01-26T03:47:52.799970-07:00
model:                 gpt-4-0125-preview
simple_title:         "デバッガーの使い方"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/using-a-debugger.md"
---

{{< edit_this_page >}}

## 何となぜ？
Bashでデバッガーを使用するとは、スクリプトをテストして問題を見つけるためにツールを使用することを意味します。例えば、コードをクラッシュさせたり、こっそりと誤動作させたりするバグを捕まえることです。プログラマーがこれを行う理由は、ライブ環境で大混乱を引き起こす前にエラーを捕捉することがずっと賢いからです。

## 方法：
Bashには他の言語のような組み込みのデバッガーがありませんが、`set -x` のような組み込みコマンドを使用して何が起こっているかを追跡することができます。また、アップグレードのために、`bashdb`という適切なデバッガーを使用してコードをステップ実行することもできます。以下がその一例です：

```Bash
# set -x を使ったデバッグ
set -x
echo "デバッグを開始"
my_var="こんにちは、デバッグの世界！"
echo $my_var
set +x

# bashdbを使用
# パッケージマネージャーを使用してbashdbをインストールします（例：apt、yum、brew）。
# my_script.sh というスクリプトをデバッグ：
bashdb my_script.sh
```

`set -x` を使用して実行した時の出力：
```Bash
+ echo 'デバッグを開始'
デバッグを開始
+ my_var='こんにちは、デバッグの世界！'
+ echo 'こんにちは、デバッグの世界！'
こんにちは、デバッグの世界！
+ set +x
```

## 詳細解説
歴史的に、Bashスクリプトのデバッグはコードに `echo` ステートメントを散りばめることを意味していました。しかし、`set -x` の登場により、手動での出力なしにランタイム実行の一部を垣間見ることができるようになりました。そして、もっとコントロールを求める人のために、C/C++用のgdbデバッガーに触発された `bashdb` デバッガーが登場しました。

代替案として、`set` コマンド（`-x`、`-v`、`-e`）を超える選択肢には、解析のために出力をファイルにリダイレクトするか、静的解析のために ShellCheck のような外部ツールを使用する方法が含まれます。

実装面で言えば、`set -x` はネイティブのBashオプションであり、実行されるコマンドとその引数を印刷するため、簡単です。一方、`bashdb` は、コードのステップ実行、ブレークポイントの設定、式の評価など、より逃げるバグに対して戦うチャンスを与える機能を許可します。

## 参照
- Bash Debugger Project: http://bashdb.sourceforge.net/
- Chris JohnsonとJayant Varmaによる「Pro Bash Programming」で、高度なスクリプティング。
- 静的解析のためのShellCheck: https://www.shellcheck.net/

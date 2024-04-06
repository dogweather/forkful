---
date: 2024-01-26 03:47:52.799970-07:00
description: "\u65B9\u6CD5\uFF1A Bash\u306B\u306F\u4ED6\u306E\u8A00\u8A9E\u306E\u3088\
  \u3046\u306A\u7D44\u307F\u8FBC\u307F\u306E\u30C7\u30D0\u30C3\u30AC\u30FC\u304C\u3042\
  \u308A\u307E\u305B\u3093\u304C\u3001`set -x` \u306E\u3088\u3046\u306A\u7D44\u307F\
  \u8FBC\u307F\u30B3\u30DE\u30F3\u30C9\u3092\u4F7F\u7528\u3057\u3066\u4F55\u304C\u8D77\
  \u3053\u3063\u3066\u3044\u308B\u304B\u3092\u8FFD\u8DE1\u3059\u308B\u3053\u3068\u304C\
  \u3067\u304D\u307E\u3059\u3002\u307E\u305F\u3001\u30A2\u30C3\u30D7\u30B0\u30EC\u30FC\
  \u30C9\u306E\u305F\u3081\u306B\u3001`bashdb`\u3068\u3044\u3046\u9069\u5207\u306A\
  \u30C7\u30D0\u30C3\u30AC\u30FC\u3092\u4F7F\u7528\u3057\u3066\u30B3\u30FC\u30C9\u3092\
  \u30B9\u30C6\u30C3\u30D7\u5B9F\u884C\u3059\u308B\u3053\u3068\u3082\u3067\u304D\u307E\
  \u3059\u3002\u4EE5\u4E0B\u304C\u305D\u306E\u4E00\u4F8B\u3067\u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.211828-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
weight: 35
---

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

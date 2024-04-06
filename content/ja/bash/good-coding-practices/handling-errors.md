---
date: 2024-01-26 00:50:03.931891-07:00
description: "\u65B9\u6CD5\uFF1A Bash\u30B9\u30AF\u30EA\u30D7\u30C6\u30A3\u30F3\u30B0\
  \u306B\u304A\u3051\u308B\u30A8\u30E9\u30FC\u51E6\u7406\u306F\u3001Unix\u30B7\u30A7\
  \u30EB\u306E\u8D77\u6E90\u306B\u9061\u308A\u3001\u5805\u7262\u3067\u4FE1\u983C\u6027\
  \u306E\u9AD8\u3044\u30B9\u30AF\u30EA\u30D7\u30C8\u306F\uFF08\u4ECA\u3067\u3082\uFF09\
  \u30B7\u30B9\u30C6\u30E0\u7BA1\u7406\u3084\u81EA\u52D5\u5316\u306B\u4E0D\u53EF\u6B20\
  \u3067\u3057\u305F\u3002\u5F93\u6765\u3001Bash\u3067\u306E\u30A8\u30E9\u30FC\u306F\
  \u30B3\u30DE\u30F3\u30C9\u306E\u7D42\u4E86\u30B9\u30C6\u30FC\u30BF\u30B9\u3092\u30C1\
  \u30A7\u30C3\u30AF\u3059\u308B\u3053\u3068\u3067\u51E6\u7406\u3055\u308C\u3001\u6163\
  \u4F8B\u306B\u3088\u308A\u6210\u529F\u306F0\u3092\u3001\u5931\u6557\u306F\u975E\u30BC\
  \u30ED\u5024\u3092\u8FD4\u3059\u3088\u3046\u306B\u306A\u3063\u3066\u3044\u307E\u3059\
  \u3002\u2026"
lastmod: '2024-04-05T21:53:43.214950-06:00'
model: gpt-4-1106-preview
summary: "Bash\u306F`trap`\u30B3\u30DE\u30F3\u30C9\u3092\u7D44\u307F\u8FBC\u307F\u3067\
  \u5C0E\u5165\u3057\u3001\u30E6\u30FC\u30B6\u30FC\u304C\u69D8\u3005\u306A\u30B7\u30B0\
  \u30CA\u30EB\u3084\u30B9\u30AF\u30EA\u30D7\u30C8\u7D42\u4E86\u6642\u306B\u5B9F\u884C\
  \u3059\u308B\u30B3\u30DE\u30F3\u30C9\u3092\u6307\u5B9A\u3067\u304D\u308B\u3088\u3046\
  \u306B\u3057\u307E\u3057\u305F\u3002\u3053\u308C\u306F\u30AF\u30EA\u30FC\u30F3\u30A2\
  \u30C3\u30D7\u30BF\u30B9\u30AF\u3084\u6700\u5F8C\u306E\u624B\u6BB5\u3068\u3057\u3066\
  \u306E\u30A8\u30E9\u30FC\u51E6\u7406\u30E1\u30AB\u30CB\u30BA\u30E0\u306B\u6709\u7528\
  \u3067\u3059."
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
weight: 16
---

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

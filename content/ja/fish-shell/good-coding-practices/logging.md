---
date: 2024-01-26 01:04:01.920587-07:00
description: "\u65B9\u6CD5\uFF1A Fish\u3067\u306F\u3001\u30ED\u30B0\u8A18\u9332\u306F\
  \u6A19\u6E96\u51FA\u529B\u3068\u30A8\u30E9\u30FC\u30B9\u30C8\u30EA\u30FC\u30E0\u3092\
  \u30D5\u30A1\u30A4\u30EB\u306B\u30EA\u30C0\u30A4\u30EC\u30AF\u30C8\u3059\u308B\u3060\
  \u3051\u306E\u7C21\u5358\u306A\u3082\u306E\u306B\u306A\u308A\u307E\u3059\u3002\u30B9\
  \u30AF\u30EA\u30D7\u30C8\u306E\u958B\u59CB\u6642\u523B\u3068\u7D42\u4E86\u6642\u523B\
  \u306E\u30ED\u30B0\u30A8\u30F3\u30C8\u30EA\u3092\u4F5C\u6210\u3057\u307E\u3057\u3087\
  \u3046\u3002"
lastmod: '2024-03-13T22:44:42.746285-06:00'
model: gpt-4-1106-preview
summary: "Fish\u3067\u306F\u3001\u30ED\u30B0\u8A18\u9332\u306F\u6A19\u6E96\u51FA\u529B\
  \u3068\u30A8\u30E9\u30FC\u30B9\u30C8\u30EA\u30FC\u30E0\u3092\u30D5\u30A1\u30A4\u30EB\
  \u306B\u30EA\u30C0\u30A4\u30EC\u30AF\u30C8\u3059\u308B\u3060\u3051\u306E\u7C21\u5358\
  \u306A\u3082\u306E\u306B\u306A\u308A\u307E\u3059\u3002\u30B9\u30AF\u30EA\u30D7\u30C8\
  \u306E\u958B\u59CB\u6642\u523B\u3068\u7D42\u4E86\u6642\u523B\u306E\u30ED\u30B0\u30A8\
  \u30F3\u30C8\u30EA\u3092\u4F5C\u6210\u3057\u307E\u3057\u3087\u3046."
title: "\u30ED\u30AE\u30F3\u30B0"
weight: 17
---

## 方法：
Fishでは、ログ記録は標準出力とエラーストリームをファイルにリダイレクトするだけの簡単なものになります。スクリプトの開始時刻と終了時刻のログエントリを作成しましょう。

```fish
function log_start
  echo (date "+%Y-%m-%d %H:%M:%S") " - スクリプト開始" >> my_app.log
end

function log_end
  echo (date "+%Y-%m-%d %H:%M:%S") " - スクリプト終了" >> my_app.log
end

log_start
# ... スクリプトのタスク ...
log_end

cat my_app.log
```

これにより、`my_app.log` では以下のような結果が表示されます：

```
2023-04-01 10:35:47  - スクリプト開始
2023-04-01 10:36:02  - スクリプト終了
```

高度なログ記録には、ログレベルとメッセージのためのパラメータを持つ関数を利用することができます：

```fish
function log_message --argument message
  switch "$argv[1]"
    case 'INFO' 'WARN' 'ERROR'
      set log_level $argv[1]
    case '*'
      set log_level 'DEBUG'
  end
  set log_msg (string join " " $argv[2..-1])
  echo (date "+%Y-%m-%d %H:%M:%S") "[$log_level]" $log_msg >> my_app.log
end

log_message INFO "情報メッセージです。"
log_message ERROR "何か問題が発生しました！"
```

サンプル `my_app.log` の出力例です：
```
2023-04-01 10:35:47 [INFO] 情報メッセージです。
2023-04-01 10:35:49 [ERROR] 何か問題が発生しました！
```

## 深堀り
歴史的には、シェルスクリプトでのログ記録は多くの `echo` 命令で行われていましたが、これは確かに今でも選択肢の一つですが、より複雑なシステムの実装は挑戦になり得ます。Fishには、他のシェルやプログラミング言語のように、組み込みのログメカニズムがありませんので、自分で作成する必要がよくあります。

Fishの組み込みの `echo` コマンドに代わるログ記録用のUnixツールには、`syslog` や `logger` などがあり、システムログデーモンとのインタフェースを提供し、システム全体のイベントをログ記録するためのより統合されたアプローチとなります。

Fishのシンプルさは、ログの詳細度を扱う関数を作成することを可能にし、オンまたはオフに切り替え可能な異なるレベルを設定することができます。実装によっては、スクリプトの名前、行番号、タイムスタンプも含めることができ、イベントに至るまでのステップを遡りやすくします。

## 参照
- Fish Shell ドキュメントの関数の書き方について： https://fishshell.com/docs/current/#syntax-function
- 基本的なシェルスクリプトのヒント： https://developer.ibm.com/tutorials/l-lpic1-103-4/
- Syslogプロトコルガイド： https://tools.ietf.org/html/rfc5424

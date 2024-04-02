---
date: 2024-01-26 01:04:01.920587-07:00
description: "\u30ED\u30B0\u3068\u306F\u57FA\u672C\u7684\u306B\u3001\u30A2\u30D7\u30EA\
  \u30B1\u30FC\u30B7\u30E7\u30F3\u304C\u4F55\u3092\u3057\u3066\u3044\u308B\u304B\u3092\
  \u8A18\u9332\u3059\u308B\u3053\u3068\u3067\u3059\u30FC\u3064\u307E\u308A\u3001\u30B3\
  \u30FC\u30C9\u7528\u306E\u65E5\u8A18\u3068\u3044\u3048\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u305D\u308C\u3092\u3001\u72B6\u614B\u5909\u5316\u3001\
  \u30B7\u30B9\u30C6\u30E0\u30A4\u30D9\u30F3\u30C8\u3001\u5384\u4ECB\u306A\u30D0\u30B0\
  \u306A\u3069\u306E\u7D30\u304B\u3044\u70B9\u3092\u8FFD\u8DE1\u3057\u3001\u554F\u984C\
  \u304C\u898B\u904E\u3054\u3055\u308C\u306A\u3044\u3088\u3046\u306B\u3059\u308B\u305F\
  \u3081\u306B\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.746285-06:00'
model: gpt-4-1106-preview
summary: "\u30ED\u30B0\u3068\u306F\u57FA\u672C\u7684\u306B\u3001\u30A2\u30D7\u30EA\
  \u30B1\u30FC\u30B7\u30E7\u30F3\u304C\u4F55\u3092\u3057\u3066\u3044\u308B\u304B\u3092\
  \u8A18\u9332\u3059\u308B\u3053\u3068\u3067\u3059\u30FC\u3064\u307E\u308A\u3001\u30B3\
  \u30FC\u30C9\u7528\u306E\u65E5\u8A18\u3068\u3044\u3048\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u305D\u308C\u3092\u3001\u72B6\u614B\u5909\u5316\u3001\
  \u30B7\u30B9\u30C6\u30E0\u30A4\u30D9\u30F3\u30C8\u3001\u5384\u4ECB\u306A\u30D0\u30B0\
  \u306A\u3069\u306E\u7D30\u304B\u3044\u70B9\u3092\u8FFD\u8DE1\u3057\u3001\u554F\u984C\
  \u304C\u898B\u904E\u3054\u3055\u308C\u306A\u3044\u3088\u3046\u306B\u3059\u308B\u305F\
  \u3081\u306B\u884C\u3044\u307E\u3059\u3002"
title: "\u30ED\u30AE\u30F3\u30B0"
weight: 17
---

## 何となぜ？
ログとは基本的に、アプリケーションが何をしているかを記録することですーつまり、コード用の日記といえます。プログラマーはそれを、状態変化、システムイベント、厄介なバグなどの細かい点を追跡し、問題が見過ごされないようにするために行います。

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

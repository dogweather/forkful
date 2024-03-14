---
date: 2024-01-26 01:08:59.579545-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.863913-06:00'
model: gpt-4-1106-preview
summary: "\u2026"
title: "\u30ED\u30AE\u30F3\u30B0"
---

{{< edit_this_page >}}

## 何となぜ？
プログラミングにおけるログとは、アプリケーションの日記のようなものです。それは、アプリケーションが何をしているのか、どのように振る舞っているのかを理解するための、イベントやメッセージ、データポイントの体系的な記録です。プログラマーがログを取るのは、デバッグ、アプリケーションの健全性の監視、大問題に発展する前に潜在的な問題についての手がかりを得るために重要だからです。

## 使い方：
Rubyには`Logger`という名前の組み込みモジュールがあり、非常に簡単に使うことができます。始めるための簡単な例を以下に示します：

```ruby
require 'logger'

# 標準出力に出力するLoggerを作成
logger = Logger.new(STDOUT)
logger.level = Logger::INFO

# ログメッセージの例
logger.info("This is an info message")
logger.warn("This is a warning message")
logger.error("This is an error message")
```

上記のスクリプトを実行すると、次のような出力が得られます：

```
I, [2023-03-15T10:00:00.123456 #1234]  INFO -- : This is an info message
W, [2023-03-15T10:00:01.234567 #1234]  WARN -- : This is a warning message
E, [2023-03-15T10:00:02.345678 #1234] ERROR -- : This is an error message
```

ログのフォーマットやレベルを設定して余計なノイズを除外したり、ログをファイルや外部ロギングサービスなど異なる出力に向けることができます。

## 詳細な潜入
ログはプログラミングにおいて古くからの伝統のようなものです。歴史的に、ログは`grep`のようなツールで手動で解析されるシンプルなテキストファイルでした。しかし、この概念は、Log4j、LinuxのSyslog、クラウド時代のSematextやLogglyのような堅牢なログフレームワークやサービスの全体的なエコシステムへと発展しました。

Rubyの`Logger`は、始めるための飾り気のない方法ですが、もしより多くの馬力と柔軟性が必要なら、LogrageやSemantic Loggerのような代替品をチェックすると良いでしょう。これらのライブラリはRubyアプリケーションとよく連携し、ログフォーマットのより細かい制御（JSONフォーマットの構造化ログを含む）、より良いパフォーマンス、他のサービスとのシームレスな統合を提供します。

各Rubyログライブラリはそれぞれ独自の方法を持っていますが、裏側では、メッセージを送るロガーインスタンスというアイデアを中心に展開しています。ロガーは設定されたレベル—DEBUG, INFO, WARN, ERROR, FATAL, UNKNOWN—に基づいてこれらのメッセージを処理し、それらをどうするかを決定します：印刷する、ファイルに保存する、ネットワークを通じて送信するなど。

## 参照
Rubyの組み込みログモジュールについて詳しく知りたい場合は、公式ドキュメントをチェックしてください：

もしより高度なロギングに興味があるか、サードパーティのgemを探求したい場合は：
- [Lograge](https://github.com/roidrage/lograge)

一般的なログ取りの実践や哲学について（Ruby特有ではない）、これらの記事は時間を超えた読み物です：
- [Googleのサイト信頼性エンジニアリングブック - 第16章：過負荷の処理](https://sre.google/sre-book/handling-overload/#log-messages)
- [The 12 Factor App - Logs](https://12factor.net/logs)

---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:25.015320-07:00
description: "\u65B9\u6CD5\uFF1A Go\u3067\u306F\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u30D1\u30C3\u30B1\u30FC\u30B8`log`\u3092\u4F7F\u7528\u3057\u3066\u30ED\
  \u30B0\u8A18\u9332\u3092\u5B9F\u88C5\u3067\u304D\u307E\u3059\u3002\u3053\u306E\u30D1\
  \u30C3\u30B1\u30FC\u30B8\u306F\u3001\u6A19\u6E96\u51FA\u529B\u3084\u30D5\u30A1\u30A4\
  \u30EB\u3078\u306E\u66F8\u304D\u8FBC\u307F\u306A\u3069\u3001\u5358\u7D14\u306A\u30ED\
  \u30B0\u8A18\u9332\u6A5F\u80FD\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u6A19\u6E96\
  \u51FA\u529B\u3078\u306E\u57FA\u672C\u7684\u306A\u30ED\u30B0\u8A18\u9332\u306E\u4F8B\
  \u304B\u3089\u59CB\u3081\u307E\u3057\u3087\u3046\uFF1A."
lastmod: '2024-03-13T22:44:41.397527-06:00'
model: gpt-4-0125-preview
summary: "Go\u3067\u306F\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u30D1\u30C3\
  \u30B1\u30FC\u30B8`log`\u3092\u4F7F\u7528\u3057\u3066\u30ED\u30B0\u8A18\u9332\u3092\
  \u5B9F\u88C5\u3067\u304D\u307E\u3059\u3002\u3053\u306E\u30D1\u30C3\u30B1\u30FC\u30B8\
  \u306F\u3001\u6A19\u6E96\u51FA\u529B\u3084\u30D5\u30A1\u30A4\u30EB\u3078\u306E\u66F8\
  \u304D\u8FBC\u307F\u306A\u3069\u3001\u5358\u7D14\u306A\u30ED\u30B0\u8A18\u9332\u6A5F\
  \u80FD\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u6A19\u6E96\u51FA\u529B\u3078\u306E\
  \u57FA\u672C\u7684\u306A\u30ED\u30B0\u8A18\u9332\u306E\u4F8B\u304B\u3089\u59CB\u3081\
  \u307E\u3057\u3087\u3046\uFF1A."
title: "\u30ED\u30AE\u30F3\u30B0"
weight: 17
---

## 方法：
Goでは、標準ライブラリパッケージ`log`を使用してログ記録を実装できます。このパッケージは、標準出力やファイルへの書き込みなど、単純なログ記録機能を提供します。標準出力への基本的なログ記録の例から始めましょう：

```go
package main

import (
	"log"
)

func main() {
	log.Println("This is a basic log entry.")
}
```

出力：
```
2009/11/10 23:00:00 This is a basic log entry.
```

ログエントリの先頭にあるタイムスタンプは、`log`パッケージによって自動的に追加されます。次に、標準出力ではなくファイルにログを記録する方法を探ってみましょう：

```go
package main

import (
	"log"
	"os"
)

func main() {
	file, err := os.OpenFile("app.log", os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0666)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	log.SetOutput(file)
	log.Println("This log entry goes to a file.")
}
```

さて、もう少し高度な使用例を実装しましょう：ログフォーマットのカスタマイズです。Goでは、`log.New()`でカスタムロガーを作成することができます：

```go
package main

import (
	"log"
	"os"
)

func main() {
	logger := log.New(os.Stdout, "CUSTOM LOG: ", log.Ldate|log.Ltime|log.Lshortfile)
	logger.Println("This is a custom log message.")
}
```

出力：
```
CUSTOM LOG: 2009/11/10 23:00:00 main.go:11: This is a custom log message.
```

この例では、各ログメッセージに「CUSTOM LOG: 」というプレフィックスを付け、日付、時間、およびソースファイルの場所を含めています。

## 深い掘り下げ
Go標準ライブラリの`log`パッケージは直感的で多くのアプリケーションに十分ですが、構造化ログ、ログローテーション、レベルベースのログ記録など、サードパーティのログ記録ライブラリに見られるより洗練された機能が欠けています。`zap`や`logrus`のようなパッケージは、これらの高度な機能を提供し、Goコミュニティ内でそのパフォーマンスと柔軟性に関して高く評価されています。

例えば、構造化ログ記録では、データを構造化された形式（JSONなど）でログ記録することができ、これはログがさまざまなツールやサービスによって分析されるかもしれない現代のクラウドベースのアプリケーションにとって特に有用です。特に`zap`は、その高いパフォーマンスと低い割り当てオーバーヘッドで知られ、速度と効率が重要なアプリケーションに適しています。

歴史的に見ると、Goにおけるログ記録は、言語の誕生以来、大きく進化しました。Goの初期バージョンでは、`log`パッケージで見られる基本的なログ記録機能が提供されていました。しかしながら、言語の人気が高まり、Goで書かれたアプリケーションの複雑さが増すにつれて、コミュニティはニーズに応えるためにより洗練されたログ記録ライブラリを開発し始めました。今日では、標準の`log`パッケージはシンプルなアプリケーションに対して有効な選択肢として残っていますが、多くの開発者はより複雑なログ記録要件のためにこれらのサードパーティのソリューションに目を向けています。

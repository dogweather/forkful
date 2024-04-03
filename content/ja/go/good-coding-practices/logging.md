---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:25.015320-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.397527-06:00'
model: gpt-4-0125-preview
summary: "\u30BD\u30D5\u30C8\u30A6\u30A7\u30A2\u958B\u767A\u306B\u304A\u3051\u308B\
  \u30ED\u30B0\u8A18\u9332\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u5B9F\u884C\
  \u60C5\u5831\u3092\u8A18\u9332\u3059\u308B\u904E\u7A0B\u3067\u3042\u308A\u3001\u305D\
  \u306E\u632F\u308B\u821E\u3044\u3092\u8FFD\u8DE1\u3057\u554F\u984C\u3092\u8A3A\u65AD\
  \u3059\u308B\u305F\u3081\u306B\u8A2D\u8A08\u3055\u308C\u3066\u3044\u307E\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3001\u30BD\u30D5\u30C8\u30A6\u30A7\u30A2\u306E\
  \u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u3092\u76E3\u8996\u3057\u3001\u30A8\u30E9\
  \u30FC\u3092\u30C7\u30D0\u30C3\u30B0\u3057\u3001\u30B7\u30B9\u30C6\u30E0\u306E\u30BB\
  \u30AD\u30E5\u30EA\u30C6\u30A3\u3068\u30B3\u30F3\u30D7\u30E9\u30A4\u30A2\u30F3\u30B9\
  \u3092\u78BA\u4FDD\u3059\u308B\u305F\u3081\u306B\u30ED\u30B0\u8A18\u9332\u3092\u5B9F\
  \u88C5\u3057\u3001\u3053\u308C\u3092\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\
  \u306E\u4FDD\u5B88\u3068\u5206\u6790\u306B\u4E0D\u53EF\u6B20\u306A\u30C4\u30FC\u30EB\
  \u3068\u3057\u3066\u3044\u307E\u3059\u3002."
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

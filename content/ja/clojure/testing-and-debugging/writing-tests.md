---
aliases:
- /ja/clojure/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:06.227556-07:00
description: "Clojure\u3067\u306E\u30C6\u30B9\u30C8\u4F5C\u6210\u306F\u3001\u4ED6\u306E\
  \u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u3068\u540C\u69D8\u306B\u3001\
  \u4E3B\u8981\u306A\u30B3\u30FC\u30C9\u30D9\u30FC\u30B9\u304C\u671F\u5F85\u901A\u308A\
  \u306B\u52D5\u4F5C\u3059\u308B\u304B\u3092\u691C\u8A3C\u3059\u308B\u305F\u3081\u306E\
  \u5C02\u7528\u30B3\u30FC\u30C9\u3092\u4F5C\u6210\u3059\u308B\u3053\u3068\u3092\u542B\
  \u307F\u307E\u3059\u3002\u3053\u308C\u306F\u6B63\u78BA\u6027\u3092\u78BA\u4FDD\u3059\
  \u308B\u306E\u306B\u5F79\u7ACB\u3061\u3001\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\
  \u30B0\u3092\u5BB9\u6613\u306B\u3057\u3001\u30B3\u30FC\u30C9\u306E\u5B89\u5B9A\u6027\
  \u3092\u9AD8\u3081\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.607198
model: gpt-4-0125-preview
summary: "Clojure\u3067\u306E\u30C6\u30B9\u30C8\u4F5C\u6210\u306F\u3001\u4ED6\u306E\
  \u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u3068\u540C\u69D8\u306B\u3001\
  \u4E3B\u8981\u306A\u30B3\u30FC\u30C9\u30D9\u30FC\u30B9\u304C\u671F\u5F85\u901A\u308A\
  \u306B\u52D5\u4F5C\u3059\u308B\u304B\u3092\u691C\u8A3C\u3059\u308B\u305F\u3081\u306E\
  \u5C02\u7528\u30B3\u30FC\u30C9\u3092\u4F5C\u6210\u3059\u308B\u3053\u3068\u3092\u542B\
  \u307F\u307E\u3059\u3002\u3053\u308C\u306F\u6B63\u78BA\u6027\u3092\u78BA\u4FDD\u3059\
  \u308B\u306E\u306B\u5F79\u7ACB\u3061\u3001\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\
  \u30B0\u3092\u5BB9\u6613\u306B\u3057\u3001\u30B3\u30FC\u30C9\u306E\u5B89\u5B9A\u6027\
  \u3092\u9AD8\u3081\u307E\u3059\u3002"
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## 何となぜ？
Clojureでのテスト作成は、他のプログラミング言語と同様に、主要なコードベースが期待通りに動作するかを検証するための専用コードを作成することを含みます。これは正確性を確保するのに役立ち、リファクタリングを容易にし、コードの安定性を高めます。

## 方法
ClojureはJVMを活用しており、さまざまなテストフレームワークをサポートしています。しかし、一般的に使用される組み込みライブラリは`clojure.test`です。こちらが簡単な例です：

```clojure
(ns example.test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest test-addition
  (testing "加算機能"
    (is (= 4 (add 2 2)))
    (is (= 7 (add 3 4)))))

(run-tests)
```

このテストを実行した後、以下のような出力が表示されます：

```
Testing example.test

Ran 2 tests containing 2 assertions.
0 failures, 0 errors.
```

より多くの機能を求める人は、`Midje` や `test.check` などのサードパーティライブラリを利用することができます。以下はMidjeを使用した同様のテストの例です：

まず、プロジェクト.cljの依存関係にMidjeを追加します：
```clojure
[midje "1.9.9"]
```

次に、Midjeを使用したテストは以下のようになります：

```clojure
(ns example.test
  (:require [midje.sweet :refer :all]
            [example.core :refer :all]))

(fact "加算のテスト"
  (add 2 2) => 4
  (add 3 4) => 7)
```

`lein midje`を使ってテストを実行すると、出力は以下のようなものになります：

```
All checks (2) succeeded.
```

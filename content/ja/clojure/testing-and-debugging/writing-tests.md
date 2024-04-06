---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:06.227556-07:00
description: "\u65B9\u6CD5 Clojure\u306FJVM\u3092\u6D3B\u7528\u3057\u3066\u304A\u308A\
  \u3001\u3055\u307E\u3056\u307E\u306A\u30C6\u30B9\u30C8\u30D5\u30EC\u30FC\u30E0\u30EF\
  \u30FC\u30AF\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\u3059\u3002\u3057\
  \u304B\u3057\u3001\u4E00\u822C\u7684\u306B\u4F7F\u7528\u3055\u308C\u308B\u7D44\u307F\
  \u8FBC\u307F\u30E9\u30A4\u30D6\u30E9\u30EA\u306F`clojure.test`\u3067\u3059\u3002\
  \u3053\u3061\u3089\u304C\u7C21\u5358\u306A\u4F8B\u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:37:49.892368-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5 Clojure\u306FJVM\u3092\u6D3B\u7528\u3057\u3066\u304A\u308A\u3001\
  \u3055\u307E\u3056\u307E\u306A\u30C6\u30B9\u30C8\u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\
  \u30AF\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\u3059\u3002\u3057\u304B\
  \u3057\u3001\u4E00\u822C\u7684\u306B\u4F7F\u7528\u3055\u308C\u308B\u7D44\u307F\u8FBC\
  \u307F\u30E9\u30A4\u30D6\u30E9\u30EA\u306F`clojure.test`\u3067\u3059\u3002\u3053\
  \u3061\u3089\u304C\u7C21\u5358\u306A\u4F8B\u3067\u3059\uFF1A."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

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

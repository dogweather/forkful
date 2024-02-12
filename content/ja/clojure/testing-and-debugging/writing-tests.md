---
title:                "テストの作成"
aliases:
- /ja/clojure/writing-tests/
date:                  2024-02-03T19:30:06.227556-07:00
model:                 gpt-4-0125-preview
simple_title:         "テストの作成"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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

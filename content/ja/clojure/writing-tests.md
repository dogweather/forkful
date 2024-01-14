---
title:    "Clojure: テストを書く"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## なぜテストを書くのか

テストは、プログラムを安定させるために必要です。バグを修正する際に、テストがあればそれらを再現することができ、より確実に修正することができます。

## 作り方

テストを書くには、いくつかのステップがあります。

まずは、[Leiningen](https://leiningen.org/)を使ってプロジェクトを作成します。

```Clojure
lein new app example
```

テスト用のファイルを作成してから、[Midje](https://github.com/marick/Midje)を依存関係として追加します。

```Clojure
(ns example.core-test
  (:require [midje.sweet :refer :all]
           [example.core :refer :all]))
```

Midjeのドキュメントを参考に、サンプルテストを書いてみましょう。

```Clojure
(fact "3の2乗は9になる"
  (square 3) => 9)

(fact "0の2乗は0になる"
  (square 0) => 0)
```

テストを実行するには、以下のコマンドを実行します。

```Clojure
lein midje
```

もしテストが失敗した場合は、そのファイルと行数が表示されるので、修正を行い再実行してください。

## テストについての深堀り

テストを書く際には、プログラムの振る舞いを理解することが重要です。それぞれのテストケースが必要な要件を満たしているかを確認するために、テストケースをいくつか想定してみると良いでしょう。また、テストを書く際には、コンパイルエラーやデバッグプリントなどの補助的な手段を利用することも大切です。

## 参考リンク

- [Midje公式ドキュメント](https://github.com/marick/Midje/wiki)
- [Clojureのテスト概要](https://clojure.org/guides/testing)
- [Leiningen公式サイト](https://leiningen.org/)

## 関連記事

- [Clojureでの単体テストのヒント](https://engineering.21buttons.com/unit-testing-in-clojure-c48a27cb3d2d)
- [テストドリブン開発の基本](https://medium.freecodecamp.org/test-driven-development-101-with-clojure-dd7fab9daf9)
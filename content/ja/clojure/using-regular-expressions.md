---
title:    "Clojure: 正規表現を使用する"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## なぜ

正規表現を使用する理由は何でしょうか？正規表現は、文字列を操作する際に非常に便利なツールです。特定のパターンを持つ文字列を検索・置換することができるため、データの前処理やバリデーションなど、様々な場面で役立ちます。

## 方法

正規表現をClojureでどのように使用するかを見ていきましょう。まずは```re-matches```関数を使用して、文字列が特定のパターンを満たしているかどうかを確認してみましょう。

```Clojure
;; 文字列が数字のみで構成されているかどうかを調べる
(re-matches #"\d+" "1234") ;; => "1234"
(re-matches #"\d+" "abcde") ;; => nil
```

次に、```re-find```関数を使用して、文字列内の特定のパターンにマッチする部分を抽出することができます。

```Clojure
;; 文字列から数字の部分だけを抽出する
(re-find #"\d+" "今日は1月13日です") ;; => "1"
```

さらに、正規表現を使用して文字列を分割することも可能です。```re-seq```関数を使用することで、指定したパターンにマッチする部分で文字列を分割することができます。

```Clojure
;; 文字列を空白で分割する
(re-seq #"\s+" "Hello World") ;; => ("Hello" "World")
```

## 深堀り

正規表現では、様々な表記方法やオプションがあります。例えば、大文字小文字を区別せずに検索する```(?i)```オプションや、一致した部分を置換する際に置換元の文字列を使用する```$&```表記などがあります。

また、正規表現では文字クラスを使用することで、特定の文字の範囲を指定することもできます。例えば、アルファベットの小文字を表す```[a-z]```や数字を表す```[0-9]```などがあります。

さらに、正規表現ではグループ化を行うこともできます。これにより、一致した部分を後で取り出すことができるようになります。例えば、ハイフンで区切られた郵便番号を取り出す場合は、```(\d{3})-(\d{4})```のようにグループ化を行って後からそれぞれの部分を取り出すことができます。

## 参考文献

- [正規表現チートシート](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [Clojure正規表現ドキュメント](https://clojure.org/reference/regular_expressions)
- [正規表現クックブック](https://github.com/leventov/Kleene-s-Music-and-Dance-of-Regexes/blob/master/StackOverflow%20questions/Regexes%20cookbook.md#coding)

## 参考になるリンク

- [正規表現メソッド一覧](https://clojuredocs.org/clojure.string/supported-functions)
- [正規表現テストするサイト](https://regexr.com/)
- [正規表現チュートリアル動画](https://www.youtube.com/watch?v=sa-TUpSx1JA)
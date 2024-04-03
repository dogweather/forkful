---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:46.392488-07:00
description: "\u4F7F\u3044\u65B9\uFF1A Lisp\u30D5\u30A1\u30DF\u30EA\u30FC\u306E\u30EB\
  \u30FC\u30C4\u306B\u5FE0\u5B9F\u306AClojure\u306F\u3001Java\u306E\u6B63\u898F\u8868\
  \u73FE\u6A5F\u80FD\u3068\u30B7\u30FC\u30E0\u30EC\u30B9\u306B\u9023\u643A\u3059\u308B\
  \u8C4A\u5BCC\u306A\u95A2\u6570\u30BB\u30C3\u30C8\u3092\u63D0\u4F9B\u3057\u3066\u3044\
  \u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\u305D\u308C\u3089\u3092\u3069\u306E\
  \u3088\u3046\u306B\u6D3B\u7528\u3059\u308B\u304B\u3092\u8AAC\u660E\u3057\u307E\u3059\
  \uFF1A #."
lastmod: '2024-03-13T22:44:41.544226-06:00'
model: gpt-4-0125-preview
summary: "Lisp\u30D5\u30A1\u30DF\u30EA\u30FC\u306E\u30EB\u30FC\u30C4\u306B\u5FE0\u5B9F\
  \u306AClojure\u306F\u3001Java\u306E\u6B63\u898F\u8868\u73FE\u6A5F\u80FD\u3068\u30B7\
  \u30FC\u30E0\u30EC\u30B9\u306B\u9023\u643A\u3059\u308B\u8C4A\u5BCC\u306A\u95A2\u6570\
  \u30BB\u30C3\u30C8\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\u3053\u3053\
  \u3067\u306F\u3001\u305D\u308C\u3089\u3092\u3069\u306E\u3088\u3046\u306B\u6D3B\u7528\
  \u3059\u308B\u304B\u3092\u8AAC\u660E\u3057\u307E\u3059\uFF1A\n\n#."
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## 使い方：
Lispファミリーのルーツに忠実なClojureは、Javaの正規表現機能とシームレスに連携する豊富な関数セットを提供しています。ここでは、それらをどのように活用するかを説明します：

### 基本的なマッチング
文字列がパターンにマッチするかどうかを確認するには、`re-matches`を使用します。成功した場合はマッチ全体を、それ以外の場合は`nil`を返します。

```clojure
(re-matches #"\d+" "123")  ;=> "123"
(re-matches #"\d+" "abc")  ;=> nil
```

### パターンの検索
パターンの最初の出現を見つけるには、`re-find`が最適な関数です：

```clojure
(re-find #"\d+" "Order 123")  ;=> "123"
```

### キャプチャグループ
パターンに括弧を使用すると、`re-find`を使用してグループをキャプチャできます：

```clojure
(let [[_ area code] (re-find #"(1)?(\d{3})" "Phone: 123-4567")]
  (println "Area Code:" area "Code:" code))
;; 出力: Area Code: nil Code: 123
```

### グローバルサーチ（全てのマッチを見つける）
Clojureには、いくつかの言語のような組み込みのグローバル検索はありません。代わりに、全てのマッチの遅延シーケンスを得るために`re-seq`を使用します：

```clojure
(re-seq #"\d+" "id: 123, qty: 456")  ;=> ("123" "456")
```

### 文字列の分割
パターンに基づいて文字列を分割するには、`clojure.string/split`を使用します：

```clojure
(clojure.string/split "John,Doe,30" #",")  ;=> ["John" "Doe" "30"]
```

### 置換
パターンに一致する文字列の部分を`clojure.string/replace`で置換します：

```clojure
(clojure.string/replace "2023-04-01" #"\d{4}" "YYYY")  ;=> "YYYY-04-01"
```

### サードパーティのライブラリ
Clojureの組み込みサポートでほとんどのケースに対応可能ですが、より複雑なシナリオの場合は、堅牢なデータ検証のための`clojure.spec`や、正規表現に基づくルーティングと入力検証を備えたウェブアプリケーションでのリアクティブDOM操作のための`reagent`といったライブラリの使用を検討してください。

```clojure
;; emailの検証にclojure.specを使用した例
(require '[clojure.spec.alpha :as s])
(s/def ::email (s/and string? #(re-matches #".+@.+\..+" %)))
(s/valid? ::email "test@example.com")  ;=> true
```

覚えておいてください、正規表現は強力ですが、コードを読みづらくし、保守が困難にすることもあります。それらを慎重に使用し、可能な場合はより単純な文字列操作関数を常に検討してください。

---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:46.392488-07:00
description: "\u6B63\u898F\u8868\u73FE\u306F\u3001\u5165\u529B\u306E\u691C\u8A3C\u3001\
  \u691C\u7D22\u3001\u30C6\u30AD\u30B9\u30C8\u306E\u7F6E\u63DB\u306A\u3069\u306E\u30C6\
  \u30AD\u30B9\u30C8\u51E6\u7406\u30BF\u30B9\u30AF\u306B\u4E0D\u53EF\u6B20\u306A\u3001\
  \u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\u30F3\u30B0\u3068\u30C7\u30FC\u30BF\u64CD\
  \u4F5C\u306E\u305F\u3081\u306E\u5F37\u529B\u306A\u30C4\u30FC\u30EB\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u8907\u96D1\u306A\u6587\u5B57\u5217\
  \u89E3\u6790\u3068\u30C7\u30FC\u30BF\u691C\u8A3C\u30BF\u30B9\u30AF\u3092\u52B9\u7387\
  \u7684\u304B\u3064\u7C21\u6F54\u306B\u6271\u3046\u305F\u3081\u306B\u3001\u5E83\u304F\
  \u4F7F\u7528\u3057\u3066\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.544226-06:00'
model: gpt-4-0125-preview
summary: "\u6B63\u898F\u8868\u73FE\u306F\u3001\u5165\u529B\u306E\u691C\u8A3C\u3001\
  \u691C\u7D22\u3001\u30C6\u30AD\u30B9\u30C8\u306E\u7F6E\u63DB\u306A\u3069\u306E\u30C6\
  \u30AD\u30B9\u30C8\u51E6\u7406\u30BF\u30B9\u30AF\u306B\u4E0D\u53EF\u6B20\u306A\u3001\
  \u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\u30F3\u30B0\u3068\u30C7\u30FC\u30BF\u64CD\
  \u4F5C\u306E\u305F\u3081\u306E\u5F37\u529B\u306A\u30C4\u30FC\u30EB\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u8907\u96D1\u306A\u6587\u5B57\u5217\
  \u89E3\u6790\u3068\u30C7\u30FC\u30BF\u691C\u8A3C\u30BF\u30B9\u30AF\u3092\u52B9\u7387\
  \u7684\u304B\u3064\u7C21\u6F54\u306B\u6271\u3046\u305F\u3081\u306B\u3001\u5E83\u304F\
  \u4F7F\u7528\u3057\u3066\u3044\u307E\u3059\u3002"
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## 何となく？なぜ？
正規表現は、入力の検証、検索、テキストの置換などのテキスト処理タスクに不可欠な、パターンマッチングとデータ操作のための強力なツールです。プログラマーは、複雑な文字列解析とデータ検証タスクを効率的かつ簡潔に扱うために、広く使用しています。

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

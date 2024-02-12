---
title:                "文字列を小文字に変換"
aliases: - /ja/clojure/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:17.903069-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列を小文字に変換するとは、プログラム上で全ての大文字を小文字に一括して変えることです。これは、大文字・小文字を区別しない検索や、データの整形・標準化の際に役立ちます。

## How to: (方法)
Clojureでは`clojure.string/lower-case`関数を使って文字列を小文字に変えます。

```clojure
(require '[clojure.string :as str])

;; 文字列を小文字に変換
(str/lower-case "Hello, World!")
;; => "hello, world!"
```

シンプルですね。試してみてください。

## Deep Dive (掘り下げ)
Clojureでの小文字変換はJavaの`toLowerCase`を裏側で使います。これはUnicode標準に従っているため、多言語に対応しています。

以前のバージョンでは独自の実装だったこともありますが、標準化と効率のためJavaのメソッドが利用されています。

他の方法？`map`関数と`Character/toLowerCase`で一文字ずつ変換することもできますが、遅く不便です。

```clojure
(apply str (map #(Character/toLowerCase %) "Hello, World!"))
;; => "hello, world!"
```

普通は`clojure.string/lower-case`を使いましょう。

## See Also (関連情報)
- Clojureの公式ドキュメントの`clojure.string/lower-case`: https://clojuredocs.org/clojure.string/lower-case
- Unicodeの標準について: https://unicode.org/
- Javaの`toLowerCase`について: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase()
